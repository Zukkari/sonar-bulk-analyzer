package io.github.zukkari

import java.io.File
import java.nio.file.Files

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.execution._
import io.github.zukkari.git.{GitProjectCloner, GitRepositoryImpl}
import io.github.zukkari.gradle.GradleBuildFileEnhancer
import io.github.zukkari.parser.{FDroidProjectFileParserImpl, PostCloneProject, ProjectFileParser}
import io.github.zukkari.project.{NoOp, ProjectAnalyzer, ProjectBuilder, ProjectClassifier}
import io.github.zukkari.sonar.SonarClientImpl
import io.github.zukkari.sonar.`export`.{SonarExportClient, SonarIssueExporter}
import scopt.OParser

case class SonarBulkAnalyzerConfig
(
  repositoryFile: File = new File("."),
  out: File = new File("."),
  error: File = new File("."),
  parser: ProjectFileParser = new FDroidProjectFileParserImpl(context),
  sonarPluginVersion: String = "2.8",
  command: String = "",
  sonarUrl: String = "",
  sonarToken: String = "",
  defaultProfile: String = "",
  export: File = new File("exported_results"),
  paging: Int = 500,
  rulePrefix: String = ""
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
        .valueName("<dir>")
        .action((x, c) => c.copy(out = x))
        .text("Directory where projects will be cloned to"),
      opt[File]('r', "repoFile")
        .valueName("<file>")
        .action((x, c) => c.copy(repositoryFile = x))
        .text("File where to take repositories to clone from"),
      opt[String]('p', "parser")
        .valueName("<parserClass>")
        .action((x, c) => x match {
          case "FDroid" => c.copy(parser = new FDroidProjectFileParserImpl(context))
          case _ => c
        })
        .text("Parser to use when parsing repository file"),
      opt[File]('e', "error")
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
        .valueName("<profile>")
        .action((x, c) => c.copy(defaultProfile = x))
        .text("Default profile to set for Sonar projects"),
      opt[File]('x', "export")
        .valueName("<export_file>")
        .action((x, c) => c.copy(export = x))
        .text("Where to export results from SonarQube"),
      opt[Int]("paging")
        .valueName("<paging size>")
        .action((x, c) => c.copy(paging = x))
        .text("How many issues to fetch at once"),
      cmd("build")
        .action((_, c) => c.copy(command = "build"))
        .text("Build the projects in provided directory"),
      opt[String]("prefix")
        .valueName("<prefix>")
        .action((x, c) => c.copy(rulePrefix = x))
        .text("Prefixes that rules must start with"),
      cmd("export")
        .action((_, c) => c.copy(command = "export"))
        .text("Export results from SonarQube instance"),
      cmd("analyze")
        .action((_, c) => c.copy(command = "analyze"))
        .text("Analyze the projects"),
      help("help")
        .text("Display help"),
    )
  }

  def runExport(config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val client = new SonarExportClient(config)
    val exporter = new SonarIssueExporter(config)
    for {
      issues <- client.readIssues
      _ <- exporter.`export`(issues) *> IO(log.info(s"Finished export of ${issues.values.map(_.size).sum} issues"))
      _ <- IO(executor.shutdown()) *> IO(log.info("Executor shutdown finished"))
    } yield ExitCode.Success
  }

  def runAnalysis(implicit config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val (classifier, builder) = dependencies

    val client = new SonarClientImpl(config)

    val analyzer = new ProjectAnalyzer(config)

    val repositories = IO {
      config.out.listFiles((f, _) => f.isDirectory)
        .toList
        .map(dir => GitRepositoryImpl(dir.getName, PostCloneProject, dir))
    }

    for {
      repos <- repositories
      // Classify projects
      classified <- classifier.classify(repos)
      // Build the projects
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

  override def run(args: List[String]): IO[ExitCode] = {
    OParser.parse(parser, args, SonarBulkAnalyzerConfig()) match {
      case Some(config) =>
        config.command match {
          case "build" => runBuild(config)
          case "export" => runExport(config)
          case "analyze" => runAnalysis(config)
          case _ => runWith(config)
        }
      case _ => IO(log.info("Invalid configuration provided")).as(ExitCode.Error)
    }
  }

  def runWith(implicit config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val (classifier, builder) = dependencies

    val cloner = new GitProjectCloner(config)
    val client = new SonarClientImpl(config)

    val analyzer = new ProjectAnalyzer(config)
    for {
      // Load projects
      projects <- config.parser.parse(config.repositoryFile)
      // Create directory for projects if missing
      _ <- mkDir(config)
      // Clone repositories
      allProjects <- cloner.doClone(projects)
      cloned = allProjects.filter {
        case _: GitRepositoryImpl => true
        case _ => false
      }
      // Classify projects
      classified <- classifier.classify(cloned)
      // Build the projects
      builtProjects <- builder.build(classified)
      onlySuccess = builtProjects.filter(_ != NoOp)
      //Change the default profile to required profile
      _ <- client.defaultProfile
      // Create the projects in SonarQube
      _ <- client.createProjects(onlySuccess)
      // Run analysis
      _ <- analyzer.analyze(onlySuccess)
      _ <- IO(log.info("Analysis finished..."))
      _ <- IO(executor.shutdown()) *> IO(log.info("Shut down executor service"))
    } yield ExitCode.Success
  }

  def runBuild(implicit config: SonarBulkAnalyzerConfig): IO[ExitCode] = {
    val (classifier, builder) = dependencies

    val client = new SonarClientImpl(config)

    val analyzer = new ProjectAnalyzer(config)

    val repositories = IO {
      config.out.listFiles((f, _) => f.isDirectory)
        .toList
        .map(dir => GitRepositoryImpl(dir.getName, PostCloneProject, dir))
    }

    for {
      repos <- repositories
      // Classify projects
      classified <- classifier.classify(repos)
      // Build the projects
      builtProjects <- builder.build(classified)
      onlySuccess = builtProjects.filter(_ != NoOp)
      //Change the default profile to required profile
      _ <- client.defaultProfile
      // Create the projects in SonarQube
      _ <- client.createProjects(onlySuccess)
      // Run analysis
      _ <- analyzer.analyze(onlySuccess)
      _ <- IO(log.info("Analysis finished..."))
      _ <- IO(executor.shutdown()) *> IO(log.info("Shut down executor service"))
    } yield ExitCode.Success
  }

  private def dependencies(implicit config: SonarBulkAnalyzerConfig) = {
    val classifier = new ProjectClassifier(config)

    val enhancer: GradleBuildFileEnhancer = new GradleBuildFileEnhancer
    (classifier, new ProjectBuilder(config, enhancer))
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


