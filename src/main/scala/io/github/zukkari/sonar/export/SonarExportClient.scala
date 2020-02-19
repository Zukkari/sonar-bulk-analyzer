package io.github.zukkari.sonar.`export`

import cats.data.Reader
import cats.effect.{ContextShift, IO, Resource}
import com.typesafe.scalalogging.Logger
import io.circe.{Json, ParsingFailure}
import io.github.zukkari.SonarBulkAnalyzerConfig
import org.http4s.client.dsl.Http4sClientDsl
import io.github.zukkari.execution._
import org.http4s.{BasicCredentials, Uri}
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.headers.Authorization
import org.http4s.dsl.io._
import io.circe.parser._
import cats.implicits._


class SonarExportClient(val config: SonarBulkAnalyzerConfig) extends Http4sClientDsl[IO] {
  private val log = Logger(classOf[SonarExportClient])

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def readIssues: IO[List[SonarIssue]] = {
    val issueCountReader = mkIssueCountReader
    val issueReader = issueListReader

    val parsed = Uri.fromString(config.sonarUrl)
    parsed match {
      case Left(error) => IO.raiseError(error)
      case Right(uri) =>
        val extended = uri / "api" / "issues" / "search"
        log.info(s"Getting issue count from URI: $extended")

        client().use {
          client =>
            makeRequest(client, extended)
              .flatMap {
                case Left(failure) => IO.raiseError(failure)
                case Right(json) => IO(issueCountReader.run(json))
              }
              .flatMap { issueCount: Int =>
                val runs = (issueCount / config.paging.toDouble).toInt
                (1 to runs).map {
                  page =>
                    val issueUri = (uri / "api" / "issues" / "search")
                      .withQueryParam("p", page)
                      .withQueryParam("ps", config.paging)

                    log.info(s"Fetching page $page with URI: $issueUri")

                    makeRequest(client, issueUri)
                      .flatMap {
                        case Left(failure) => IO.raiseError(failure)
                        case Right(json) => IO(issueReader.run(json))
                      }
                }.toList
                  .traverse(identity)
                  .map(list => list.foldRight(List.empty[SonarIssue])(_ ++ _))
              }
        }
    }
  }

  private def makeRequest(client: Client[IO], issueUri: Uri): IO[Either[ParsingFailure, Json]] = {
    client.expect[String](
      GET(issueUri,
        Authorization(BasicCredentials(config.sonarToken, "")))
    ).map(parse)
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

        SonarIssue(rule, project)
    }
  }

  private def client(): Resource[IO, Client[IO]] = BlazeClientBuilder[IO](context).resource
}
