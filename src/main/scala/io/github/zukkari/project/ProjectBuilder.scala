package io.github.zukkari.project

import java.util.concurrent.Executors

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger

import scala.concurrent.ExecutionContext

class ProjectBuilder {
  private val log = Logger(this.getClass)

  private val context = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(10))
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def build(projects: List[ProjectBuilderKind]): IO[Unit] = {
    projects.map {
      case NoOp => IO.unit
      case p => p.build
    }
      .parSequence
      .map(projects => IO {
        log.info(s"Finished building ${projects.size} projects")
      } *> IO.unit)
  }
}
