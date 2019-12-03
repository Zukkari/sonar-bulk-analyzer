package io.github.zukkari.parser

case class Project
(
  name: String,
  description: String,
  url: String
)

trait ProjectFileParser {
  def parse(line: String): Option[Project]
}

class FDroidProjectFileParserImpl extends ProjectFileParser {
  private val lineParseRegex = "\\|\\s+\\d+\\.\\s+(.+?)\\|\\s+(.+?)\\|\\s*(.+?)\\|".r("name", "description", "url")

  override def parse(line: String): Option[Project] = ???

}
