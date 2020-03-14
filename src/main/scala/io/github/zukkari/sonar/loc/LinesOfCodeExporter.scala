package io.github.zukkari.sonar.loc

import java.io.PrintWriter
import java.nio.file.Files

import cats.data.Reader
import cats.effect.{ContextShift, IO, Resource}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.circe.Json
import io.circe.parser._
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.execution._
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.io._
import org.http4s.headers.Authorization
import org.http4s.{BasicCredentials, Uri}

class LinesOfCodeExporter(val config: SonarBulkAnalyzerConfig)
    extends Http4sClientDsl[IO] {
  private val log = Logger(classOf[LinesOfCodeExporter])

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def lines(): IO[Unit] = {
    log.info(s"Exporting lines of code into ${config.`export`}")
    for {
      path <- IO {
        val path = Files.createFile(config.`export`.toPath)
        log.info(s"File created @ ${config.`export`}")
        path
      }
      linesDescriptors <- fetchLinesOfCodeDescriptors()
      _ <- write(path, linesDescriptors)
    } yield ()
  }

  private def write(path: java.nio.file.Path,
                    descriptors: List[Option[LinesDescriptor]]): IO[Unit] = {
    writer(path).use { writer =>
      log.info(s"Writing ${descriptors.size} to @ $path")
      IO {
        writer.write("project;lines")

        descriptors.foreach {
          case None =>
            log.info("Empty row, skipping...")
          case Some(descriptor) =>
            log.info(s"Writing row: $descriptor")
            writer.write(descriptor.project ++ ";" ++ descriptor.loc.toString)
        }
      }
    }
  }

  private def writer(path: java.nio.file.Path): Resource[IO, PrintWriter] = {
    val acquire = IO {
      new PrintWriter(path.toString, "UTF-8")
    }

    Resource.fromAutoCloseable(acquire)
  }

  private def makeProjectReader: Reader[Json, Option[LinesDescriptor]] =
    Reader { json =>
      for {
        key <- json.hcursor
          .downField("component")
          .downField("key")
          .focus
          .flatMap(_.asString)

        loc <- json.hcursor
          .downField("component")
          .downField("measures")
          .focus
          .flatMap(_.asArray)
          .flatMap(_.headOption)
          .flatMap(_.hcursor.downField("value").focus.flatMap(_.asNumber))
          .flatMap(_.toInt)
        if loc > 0
      } yield LinesDescriptor(key, loc)
    }

  private def fetchProject(project: String): IO[Option[LinesDescriptor]] = {
    val projectReader = makeProjectReader
    Uri.fromString(config.sonarUrl) match {
      case Left(error) => IO.raiseError(error)
      case Right(uri) =>
        val locUri = (uri / "api" / "measures" / "component")
          .withQueryParam("metricKeys", "ncloc")
          .withQueryParam("componentKey", project)

        BlazeClientBuilder[IO](context).resource
          .use { client =>
            client.expect[String](
              GET(
                locUri,
                Authorization(BasicCredentials(config.sonarToken, ""))
              )
            )
          }
          .map(parse)
          .flatMap {
            case Left(error) => IO.raiseError(error)
            case Right(json) => IO(projectReader.run(json))
          }
    }
  }

  private def fetchLinesOfCodeDescriptors()
    : IO[List[Option[LinesDescriptor]]] = {
    val projectKeyReader = makeProjectKeyReader
    BlazeClientBuilder[IO](context).resource
      .use { client =>
        export(client)
      }
      .map(parse)
      .flatMap {
        case Left(error) => IO.raiseError(error)
        case Right(json) => IO(json)
      }
      .map { json =>
        projectKeyReader.run(json)
      }
      .flatMap { projects =>
        projects
          .map(fetchProject)
          .parSequence
      }
  }

  private def export(client: Client[IO]): IO[String] = {
    Uri.fromString(config.sonarUrl) match {
      case Left(error) => IO.raiseError(error)
      case Right(uri) =>
        val projectFetchUri = uri / "api" / "projects" / "search"
        log.info(s"Fetching project ids from $projectFetchUri")

        client.expect[String](
          GET(
            projectFetchUri,
            Authorization(BasicCredentials(config.sonarToken, ""))
          )
        )
    }
  }

  private def makeProjectKeyReader: Reader[Json, List[String]] = {
    val singleReader = Reader[Json, String] { json =>
      json.hcursor
        .downField("key")
        .focus
        .flatMap(_.asString)
        .getOrElse("")
    }

    Reader { json =>
      json.hcursor
        .downField("components")
        .focus
        .flatMap(_.asArray)
        .getOrElse(Vector.empty)
        .map(singleReader.run)
        .toList
    }
  }
}
