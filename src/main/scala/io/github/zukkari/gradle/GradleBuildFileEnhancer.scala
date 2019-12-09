package io.github.zukkari.gradle

import java.io.File

import cats.effect.{IO, Resource}
import cats.implicits._
import io.github.zukkari.SonarBulkAnalyzerConfig

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}


class GradleBuildFileEnhancer(implicit val config: SonarBulkAnalyzerConfig) {

  private val linesToInsert = List(
    "plugins {",
    "    id 'org.sonarqube' version " ++ s"'${config.sonarPluginVersion}'",
    "}"
  )

  /**
   * We need to find whether or not does the project
   * build file contain 'buildscript' block.
   * If it does, then we have to add our plugin below it
   * Otherwise we can add it at the beginning of the file.
   *
   * @param f to add plugin node to
   * @return action that will enhance the build file
   */
  def enhance(f: File): IO[Unit] = {
    gradleBuildFile(f)
      .use { lines =>
        IO(lines.getLines().toList)
      }
      .map(scriptWithPluginNode)
  }

  def gradleBuildFile(f: File): Resource[IO, BufferedSource] = {
    val path = f.toPath.resolve("build.gradle")
    val input = IO {
      Source.fromFile(path.toFile)
    }

    Resource.fromAutoCloseable(input)
  }

  def indexOfClosingParen(remainder: List[String], start: Int): Int = {
    @tailrec
    def processLine(line: List[Char], parens: Int): (Int, Option[Int]) = {
      line match {
        case x :: xs =>
          x match {
            case '{' => processLine(xs, parens + 1)
            case '}' =>
              if (parens == 0) {
                (parens, start.some)
              } else {
                processLine(xs, parens - 1)
              }
            case _ => processLine(xs, parens)
          }
        case Nil => (parens, none)
      }
    }

    @tailrec
    def _indexOfClosingParen(lines: List[String], start: Int, parens: Int): Int = {
      lines match {
        case x :: xs =>
          processLine(x.toList, parens) match {
            case (_, Some(v)) => v
            case (p, _) => _indexOfClosingParen(xs, start + 1, p)
          }
        case Nil => -1
      }
    }

    _indexOfClosingParen(remainder, start, 1)
  }

  def scriptWithPluginNode(lines: List[String]): List[String] = {
    val hasBuildScript = lines.find(l => l.contains("buildscript {") || l.contains("buildscript{"))
    hasBuildScript match {
      case None => linesToInsert ++ lines
      case Some(v) =>
        val idx = lines.indexOf(v)
        val remainder = lines.drop(idx + 1)
        val closing = indexOfClosingParen(remainder, idx)

        lines.slice(0, closing) ++ linesToInsert ++ lines.slice(closing, lines.size)
    }
  }
}
