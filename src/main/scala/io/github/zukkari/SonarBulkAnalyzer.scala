package io.github.zukkari

import java.io.File

import scopt.OParser

case class SonarBulkAnalyzerConfig
(
  repositoryFile: File = new File("."),
  out: File = new File(".")
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
      help("help")
        .text("Display help"),
    )
  }

  OParser.parse(parser, args, SonarBulkAnalyzerConfig()) match {
    case Some(config) => println(config)
    case _ =>
  }
}


