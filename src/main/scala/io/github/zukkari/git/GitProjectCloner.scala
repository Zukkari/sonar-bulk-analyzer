package io.github.zukkari.git

import java.io.File
import java.nio.file.Path
import java.util.UUID

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.execution._
import io.github.zukkari.parser.Project
import org.eclipse.jgit.api.Git

trait GitRepository {
  def id: String

  def project: Project

  def dir: File
}

case class GitRepositoryImpl
(
  id: String,
  project: Project,
  dir: File
) extends GitRepository

case object NoRepository extends GitRepository {
  override def id: String = ???

  override def project: Project = ???

  override def dir: File = ???
}

class GitProjectCloner(val config: SonarBulkAnalyzerConfig) {
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  private val log = Logger(this.getClass)

  def doClone(projects: List[Project]): IO[List[GitRepository]] = {
    projects.map(p => IO(log.info(s"Cloning project: $p")) *> cloneProject(p).handleErrorWith(err => IO {
      log.error(s"ERROR when cloning git repository for project: $p", err)
    } *> IO.pure(NoRepository)))
      .parSequence
  }

  def cloneProject(project: Project): IO[GitRepository] = {
    for {
      id <- IO(UUID.randomUUID().toString)
      dir <- IO(config.out.toPath.resolve(id))
      clonedDir <- cloneRepo(project, dir)
    } yield GitRepositoryImpl(id, project, clonedDir)
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
