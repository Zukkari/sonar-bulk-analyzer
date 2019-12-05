package io.github.zukkari

import java.io.File
import java.nio.file.Files

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.git.GitProjectCloner
import io.github.zukkari.parser.{FDroidProjectFileParserImpl, ProjectFileParser}
import scopt.OParser

case class SonarBulkAnalyzerConfig
(
  repositoryFile: File = new File("."),
  out: File = new File("."),
  parser: ProjectFileParser = new FDroidProjectFileParserImpl
)

object SonarBulkAnalyzer extends IOApp {
  private val log = Logger(SonarBulkAnalyzer.getClass)

  private val builder = OParser.builder[SonarBulkAnalyzerConfig]

  private val parser = {
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
      opt[String]('p', "parser")
        .valueName("<parserClass>")
        .action((x, c) => x match {
          case "FDroid" => c.copy(parser = new FDroidProjectFileParserImpl)
          case _ => c
        })
        .text("Parser to use when parsing repository file"),
      help("help")
        .text("Display help"),
    )
  }

  override def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser, args, SonarBulkAnalyzerConfig()) match {
      case Some(config) => runWith(config)
      case _ => IO(log.info("Invalid configuration provided")).as(ExitCode.Error)
    }
  }

  def runWith(config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val cloner = new GitProjectCloner(config)

    for {
      // Load projects
      projects <- config.parser.parse(config.repositoryFile)
      // Create directory for projects if missing
      _ <- mkDir(config)
      // Clone repositories
      _ <- cloner.doClone(projects)
      _ <- IO(log.info("Analysis finished..."))
    } yield ExitCode.Success
  }

  def mkDir(config: SonarBulkAnalyzerConfig): IO[Unit] = {
    IO(log.info(s"Creating directory for repositories in '${config.out.getAbsolutePath}'")) *>
    IO {
      Files.createDirectory(config.out.toPath)
    }
  }
}


