package io.github.zukkari.git

import java.io.File
import java.nio.file.Path
import java.util.UUID

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.parser.Project
import org.eclipse.jgit.api.Git

import scala.concurrent.ExecutionContext.global

case class GitRepository
(
  project: Project,
  dir: File
)

class GitProjectCloner(val config: SonarBulkAnalyzerConfig) {
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  private val log = Logger(this.getClass)

  def doClone(projects: List[Project]): IO[List[GitRepository]] = {
    projects.map(p => IO(log.info(s"Cloning project: $p")) *> cloneProject(p))
      .parSequence
  }

  def cloneProject(project: Project): IO[GitRepository] = {
    for {
      id <- IO(UUID.randomUUID().toString)
      dir <- IO(config.out.toPath.resolve(id))
      clonedDir <- cloneRepo(project, dir)
    } yield GitRepository(project, clonedDir)
  }

  def cloneRepo(p: Project, path: Path): IO[File] = {
    IO {
      Git.cloneRepository()
        .setURI(p.url)
        .setDirectory(path.toFile)
        .call()
        .getRepository
        .getDirectory
        .getParentFile
    }
  }
}
