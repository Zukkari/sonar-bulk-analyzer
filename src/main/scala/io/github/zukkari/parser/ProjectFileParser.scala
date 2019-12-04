package io.github.zukkari.parser

import java.io.File

import cats.effect.{ContextShift, IO, Resource}
import com.typesafe.scalalogging.Logger

import scala.io.{BufferedSource, Source}
import cats.implicits._

import scala.concurrent.ExecutionContext.global

case class Project
(
  name: String,
  description: String,
  url: String
)

trait ProjectFileParser {
  def parse(f: File): IO[List[Project]]
}

sealed class ParseException(reason: String) extends Exception

class FDroidProjectFileParserImpl extends ProjectFileParser {
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(global)

  private val log = Logger(this.getClass)

  private val projectRegex = "\\|\\s+\\d+\\.\\s+(.+?)\\|\\s+(.+?)\\|\\s*(.+?)\\|".r("name", "description", "url")

  override def parse(f: File): IO[List[Project]] = {
    mkResource(f)
      .use(source => IO {
        log.info(s"Reading lines from file: ${f.getAbsolutePath}")

        source.getLines.toList
      })
      .flatMap(parseProjects)
  }

  def mkResource(f: File): Resource[IO, BufferedSource] = {
    val acquire = IO {
      log.info(s"Opening file: ${f.getAbsolutePath}")
      Source.fromFile(f)
    }

    Resource.fromAutoCloseable(acquire)
  }

  def parseProjects(projects: List[String]): IO[List[Project]] = projects.map(mkProject).parSequence

  def mkProject(line: String): IO[Project] = line match {
    case projectRegex(name, desc, url) => IO.pure(Project(name.trim, desc.trim, url.trim))
    case _ => IO.raiseError(new ParseException(s"Failed to parse line '$line' into a project"))
  }
}
