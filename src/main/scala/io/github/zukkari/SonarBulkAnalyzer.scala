package io.github.zukkari

import java.io.File

import akka.actor.{ActorSystem, Props}
import io.github.zukkari.actors.{GitSourceRouter, RepoFileParser}
import io.github.zukkari.parser.{FDroidProjectFileParserImpl, ProjectFileParser}
import scopt.OParser

import scala.concurrent.duration._

case class SonarBulkAnalyzerConfig
(
  repositoryFile: File = new File("."),
  out: File = new File("."),
  parser: ProjectFileParser = new FDroidProjectFileParserImpl
)

object SonarBulkAnalyzer extends App {
  val builder = OParser.builder[SonarBulkAnalyzerConfig]

  val parser = {
    import builder._

    OParser.sequence(
      programName("sba"),
      head("Sonar Bulk Analyzer", "0.1-beta"),
      opt[File]('o', "out")
        .required()
        .valueName("<dir>")
        .action((x, c) => c.copy(out = x))
        .text("Directory where projects will be cloned to"),
      opt[File]('r', "repoFile")
        .required()
        .valueName("<file>")
        .action((x, c) => c.copy(repositoryFile = x))
        .text("File where to take repositories to clone from"),
      opt[Unit]('p', "parser")
        .required()
        .valueName("<parserClass>")
        .action((x, c) => x match {
          case "FDroid" => c.copy(parser = new FDroidProjectFileParserImpl)
          case _ => ???
        })
        .text("Parser to use when parsing repository file"),
      help("help")
        .text("Display help"),
    )
  }

  OParser.parse(parser, args, SonarBulkAnalyzerConfig()) match {
    case Some(config) => run(config)
    case _ =>
  }

  def run(config: SonarBulkAnalyzerConfig): Unit = {
    val system = ActorSystem("SonarBulkAnalyzerSystem")

    val repoFileParser = system.actorOf(RepoFileParser.props(config.parser), "repoFileParser")
    system.actorOf(Props[GitSourceRouter], "gitSourceRouter")

    import system.dispatcher
    system.scheduler.scheduleOnce(500 millis) {
      repoFileParser ! config.repositoryFile
    }
  }
}


