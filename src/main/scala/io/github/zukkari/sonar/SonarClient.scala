package io.github.zukkari.sonar

import cats.effect.{ContextShift, IO, Resource}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.project.ProjectBuilderKind
import org.http4s.Uri
import org.http4s.client.Client
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.dsl.io._

import scala.concurrent.ExecutionContext
import scala.concurrent.ExecutionContext.global

trait SonarClient {
  def createProject(project: ProjectBuilderKind): IO[Unit]

  def createProjects(projects: List[ProjectBuilderKind]): IO[Unit]
}

class SonarClientImpl(implicit val config: SonarBulkAnalyzerConfig,
                      val context: ExecutionContext) extends SonarClient with Http4sClientDsl[IO] {
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  private val log = Logger(this.getClass)

  override def createProjects(projects: List[ProjectBuilderKind]): IO[Unit] = {
    projects.map(createProject)
      .parSequence *>
      IO {
        log.info("Finished project creation via API")
      }
  }

  override def createProject(project: ProjectBuilderKind): IO[Unit] = {
    val parsed = Uri.fromString(config.sonarUrl)
    parsed match {
      case Left(value) => IO.raiseError(value)
      case Right(uri) =>
        val extended = (uri / "api" / "projects" / "create")
          .withQueryParam("name", project.id)
          .withQueryParam("project", project.id)
        log.info(s"Creating project for uri: $extended")

        client().use { client =>
          client.expect[Unit](POST(extended))
        }
    }
  }

  private def client(): Resource[IO, Client[IO]] = BlazeClientBuilder[IO](global).resource

}
