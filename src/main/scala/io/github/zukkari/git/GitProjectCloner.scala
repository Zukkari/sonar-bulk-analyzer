package io.github.zukkari.git

import java.io.File

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.parser.Project

import scala.concurrent.ExecutionContext.global

case class GitRepository
(
  project: Project,
  dir: File
)

class GitProjectCloner(val config: SonarBulkAnalyzerConfig) {
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  private val log = Logger(this.getClass)

  def process(projects: List[Project]): IO[List[GitRepository]] = {
    projects.map(p => IO(log.info(s"Cloning project: $p")) *> IO.pure(GitRepository(p, new File("."))))
      .parSequence
  }
}
