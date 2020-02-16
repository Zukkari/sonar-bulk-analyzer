package io.github.zukkari.project

import java.io.File
import java.nio.file.Files

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.execution._

class ProjectAnalyzer(config: SonarBulkAnalyzerConfig) {
  private val log = Logger(this.getClass)

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def analyze(projects: List[ProjectBuilderKind]): IO[Unit] = {
    projects.map {
      case NoOp => IO.unit
      case project => project.runAnalysis(mkLogFile(project))
    }.parSequence *>
      IO {
        log.info(s"Finished analysis for ${projects.size} projects")
      }
  }

  def mkLogFile(project: ProjectBuilderKind): IO[File] =
    IO {
      Files.createFile(config.error.toPath.resolve(s"analysis-${project.id}")).toFile
    }
}
