package io.github.zukkari.project

import java.io.File
import java.nio.file.Files

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.gradle.GradleBuildFileEnhancer

import scala.concurrent.ExecutionContext

class ProjectBuilder(implicit val config: SonarBulkAnalyzerConfig,
                     executionContext: ExecutionContext,
                     implicit val enhancer: GradleBuildFileEnhancer) {
  private val log = Logger(this.getClass)

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(executionContext)

  def build(projects: List[ProjectBuilderKind]): IO[Unit] = {
    projects.map {
      case NoOp => IO.unit
      case m: MavenProjectBuilderKind => m.build(mkOutFile(m))
      case g: GradleProjectBuilderKind => enhancer.enhance(g.id, g.project) *> g.build(mkOutFile(g))
    }
      .parSequence
      .map(projects => IO {
        log.info(s"Finished building ${projects.size} projects")
      })
  }

  def mkOutFile(p: ProjectBuilderKind): IO[File] = {
    IO {
      val path = config.error.toPath.resolve(p.id)
      Files.createFile(path).toFile
    }
  }
}
