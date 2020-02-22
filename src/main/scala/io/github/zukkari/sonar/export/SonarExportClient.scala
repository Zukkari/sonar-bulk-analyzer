package io.github.zukkari.sonar.`export`

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


class SonarExportClient(val config: SonarBulkAnalyzerConfig) extends Http4sClientDsl[IO] {
  private val log = Logger(classOf[SonarExportClient])

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def projectNumberReader: Reader[Json, Int] = {
    Reader[Json, Int] { json =>
      json.hcursor
        .downField("paging")
        .downField("total")
        .focus
        .flatMap(_.asNumber)
        .flatMap(_.toInt)
        .getOrElse(0)
    }
  }

  def makeProjectKeyReader: Reader[Json, List[String]] = {
    val singleReader = Reader[Json, String] {
      json =>
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

  def fetchProjects(uri: Uri): IO[List[String]] = {
    val projectFetchUri = uri / "api" / "projects" / "search"

    val projectKeyReader = makeProjectKeyReader
    client().use { client =>
      log.info(s"Fetching project ids from $projectFetchUri")
      client.expect[String](
        GET(projectFetchUri,
          Authorization(BasicCredentials(config.sonarToken, "")))
      )
    }.map(parse)
      .flatMap {
        case Left(error) => IO.raiseError(error)
        case Right(json) => IO(projectNumberReader.run(json))
      }.flatMap { projectCount =>
      val runs = (projectCount / config.paging.toDouble).ceil.toInt.max(1)
      log.info(s"Total runs to do: $runs to fetch project ID-s")

      client().use { client =>
        (1 to runs).map { page =>
          val projectPageN = (uri / "api"/ "projects" / "search")
            .withQueryParam("p", page)
            .withQueryParam("ps", config.paging)

          log.info(s"Performing run $page from uri $projectPageN")

          client.expect[String](
            GET(projectPageN,
              Authorization(BasicCredentials(config.sonarToken, "")))
          ).map(parse)
            .flatMap {
              case Left(error) => IO.raiseError(error)
              case Right(json) => IO(projectKeyReader.run(json))
            }
        }.toList
          .traverse(identity)
          .map(list => list.foldRight(List.empty[String])(_ ++ _))
      }
    }
  }

  def fetchIssuesForProject(globalUri: Uri, project: String, client: Client[IO]): IO[(String, List[SonarIssue])] = {
    val uri = (globalUri / "api" / "issues" / "search")
      .withQueryParam("componentKeys", project)

    val countReader = mkIssueCountReader
    val issuesReader = issueListReader
    client.expect[String](
      GET(uri, Authorization(BasicCredentials(config.sonarToken, "")))
    ).map(parse)
      .flatMap {
        case Left(error) => IO.raiseError(error)
        case Right(json) => IO(countReader.run(json))
      }.flatMap { count =>
      log.info(s"Issues found for project $project is $count")
      val runs = (count / config.paging.toDouble).ceil.toInt.max(1)
      log.info(s"Total runs to do for project $project is $runs")

      (1 to runs).map { page =>
        val pageUri = uri.withQueryParam("p", page)
          .withQueryParam("ps", config.paging)
          .withQueryParam("componentKeys", project)

        log.info(s"Performing run $page for project $project with uri $pageUri")

        client.expect[String](
          GET(
            pageUri, Authorization(BasicCredentials(config.sonarToken, ""))
          )
        ).map(parse)
          .flatMap {
            case Left(error) => IO.raiseError(error)
            case Right(json) => IO(issuesReader.run(json))
          }
      }.toList.traverse(identity)
        .map(list => list.foldRight(List.empty[SonarIssue])(_ ++ _))
        .map(list => (project, list))
    }
  }

  def fetchIssues(uri: Uri, projects: List[String]): IO[Map[String, List[SonarIssue]]] = {
    client().use { client =>
      projects.map {
        project => fetchIssuesForProject(uri, project, client)
      }.traverse(identity)
        .map(_.toMap)
    }
  }

  def readIssues: IO[Map[String, List[SonarIssue]]] = {
    val parsed = Uri.fromString(config.sonarUrl)
    parsed match {
      case Left(error) => IO.raiseError(error)
      case Right(uri) =>
        for {
          projects <- fetchProjects(uri)
          _ <- IO(log.info(s"Fetched ${projects.size} projects"))
          projectToIssueMap <- fetchIssues(uri, projects)
        } yield projectToIssueMap
    }
  }

  private def mkIssueCountReader: Reader[Json, Int] = {
    Reader(json =>
      json.hcursor
        .downField("total")
        .focus
        .flatMap(_.asNumber)
        .flatMap(_.toInt)
        .getOrElse(0)
    )
  }

  private def issueListReader: Reader[Json, List[SonarIssue]] = {
    val singleReader = issueReader

    Reader(json =>
      json.hcursor
        .downField("issues")
        .focus
        .flatMap(_.asArray)
        .getOrElse(Vector.empty)
        .map(json => singleReader.run(json))
        .toList
    )
  }

  private def issueReader: Reader[Json, SonarIssue] = {
    Reader {
      json =>
        val rule: String = json.hcursor.downField("rule").focus.flatMap(_.asString).getOrElse("Empty rule name")
        val project: String = json.hcursor.downField("project").focus.flatMap(_.asString).getOrElse("Empty project name")
        val message: String = json.hcursor.downField("message").focus.flatMap(_.asString).getOrElse("Empty message")

        SonarIssue(rule, project, message)
    }
  }

  private def client(): Resource[IO, Client[IO]] = BlazeClientBuilder[IO](context).resource
}
