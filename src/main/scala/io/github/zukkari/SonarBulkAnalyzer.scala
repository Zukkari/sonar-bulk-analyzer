package io.github.zukkari

import java.io.File
import java.nio.file.Files
import java.util.concurrent.{ExecutorService, Executors}

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.git.{GitProjectCloner, GitRepository}
import io.github.zukkari.gradle.GradleBuildFileEnhancer
import io.github.zukkari.parser.{FDroidProjectFileParserImpl, PostCloneProject, ProjectFileParser}
import io.github.zukkari.project.{ProjectAnalyzer, ProjectBuilder, ProjectClassifier}
import io.github.zukkari.sonar.SonarClientImpl
import scopt.OParser

import scala.concurrent.ExecutionContext.global
import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

case class SonarBulkAnalyzerConfig
(
  repositoryFile: File = new File("."),
  out: File = new File("."),
  error: File = new File("."),
  parser: ProjectFileParser = new FDroidProjectFileParserImpl,
  sonarPluginVersion: String = "2.8",
  command: String = "",
  sonarUrl: String = "",
  sonarToken: String = "",
  defaultProfile: String = ""
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
      opt[File]('e', "error")
        .required()
        .valueName("<dir>")
        .action((x, c) => c.copy(error = x))
        .text("Directory to store errors and output from build proccess"),
      opt[String]('s', "sonar-version")
        .valueName("<version>")
        .action((x, c) => c.copy(sonarPluginVersion = x))
        .text("SonarQube plugin version to add to Gradle files"),
      opt[String]('t', "token")
        .required()
        .valueName("<token>")
        .action((x, c) => c.copy(sonarToken = x))
        .text("Token to use when authenticating with SonarQube"),
      opt[String]("sonar-url")
        .required()
        .valueName("<url>")
        .action((x, c) => c.copy(sonarUrl = x))
        .text("SonarQube location"),
      opt[String]("default-profile")
        .required()
        .valueName("<profile>")
        .action((x, c) => c.copy(defaultProfile = x))
        .text("Default profile to set for Sonar projects"),
      cmd("build")
        .action((_, c) => c.copy(command = "build"))
        .text("Build the projects in provided directory"),
      help("help")
        .text("Display help"),
    )
  }

  override def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser, args, SonarBulkAnalyzerConfig()) match {
      case Some(config) =>
        config.command match {
          case "build" => runBuild(config)
          case _ => runWith(config)
        }
      case _ => IO(log.info("Invalid configuration provided")).as(ExitCode.Error)
    }
  }

  def runWith(implicit config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val cloner = new GitProjectCloner
    val (classifier, executor, builder) = dependencies

    implicit val context: ExecutionContextExecutor = global
    val client = new SonarClientImpl()

    val analyzer = new ProjectAnalyzer
    for {
      // Load projects
      projects <- config.parser.parse(config.repositoryFile)
      // Create directory for projects if missing
      _ <- mkDir(config)
      // Clone repositories
      cloned <- cloner.doClone(projects)
      // Classify projects
      classified <- classifier.classify(cloned)
      // Build the projects
      _ <- builder.build(classified)
      //Change the default profile to required profile
      _ <- client.defaultProfile
      // Create the projects in SonarQube
      _ <- client.createProjects(classified)
      // Run analysis
      _ <- analyzer.analyze(classified)
      _ <- IO(log.info("Analysis finished..."))
      _ <- IO(executor.shutdown()) *> IO(log.info("Shut down executor service"))
    } yield ExitCode.Success
  }

  def runBuild(implicit config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val (classifier, executor, builder) = dependencies

    val repositories = IO {
      config.out.listFiles((f, _) => f.isDirectory)
        .toList
        .map(dir => GitRepository(dir.getName, PostCloneProject, dir))
    }

    for {
      repos <- repositories
      classified <- classifier.classify(repos)
      _ <- builder.build(classified)
      _ <- IO {
        log.info("Finished building projects")
      }
      _ <- IO(executor.shutdown()) *> IO(log.info("Shut down executor service"))
    } yield ExitCode.Success
  }

  private def dependencies(implicit config: SonarBulkAnalyzerConfig) = {
    val classifier = new ProjectClassifier(config)

    val executor: ExecutorService = Executors.newFixedThreadPool(10)
    implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)
    implicit val enhancer: GradleBuildFileEnhancer = new GradleBuildFileEnhancer
    val builder = new ProjectBuilder
    (classifier, executor, builder)
  }

  def mkDir(config: SonarBulkAnalyzerConfig): IO[Unit] = {
    IO(log.info(s"Creating directory for repositories in '${config.out.getAbsolutePath}'")) *>
      IO {
        Files.createDirectory(config.out.toPath)
      } *>
      IO {
        Files.createDirectory(config.error.toPath)
      } *>
      IO {
        log.info(s"Created error directory at => '${config.error.toPath}'")
      }
  }
}


