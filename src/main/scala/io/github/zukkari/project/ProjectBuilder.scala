package io.github.zukkari.project

import java.io.File
import java.nio.file.Files

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.execution._
import io.github.zukkari.gradle.GradleBuildFileEnhancer


class ProjectBuilder(val config: SonarBulkAnalyzerConfig,
                     val enhancer: GradleBuildFileEnhancer) {
  private val log = Logger(this.getClass)

  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def build(projects: List[ProjectBuilderKind]): IO[List[ProjectBuilderKind]] = {
    projects.map {
      case NoOp => IO {
        NoOp
      }
      case m: MavenProjectBuilderKind => m.build(mkOutFile(m))
      case g: GradleProjectBuilderKind => enhancer.enhance(g.id, g.project) *> g.build(mkOutFile(g))
    }
      .parSequence
      .flatMap(projects => IO {
        log.info(s"Finished building ${projects.size} projects")
        projects
      })
  }

  def mkOutFile(p: ProjectBuilderKind): IO[File] = {
    IO {
      val path = config.error.toPath.resolve(p.id)
      Files.createFile(path).toFile
    }
  }
}
