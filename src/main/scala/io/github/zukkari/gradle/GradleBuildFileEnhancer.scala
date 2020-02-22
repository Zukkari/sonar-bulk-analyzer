package io.github.zukkari.gradle

import java.io.{File, FileOutputStream}
import java.nio.charset.StandardCharsets

import cats.effect.{IO, Resource}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig

import scala.annotation.tailrec
import scala.io.{BufferedSource, Source}


class GradleBuildFileEnhancer(implicit val config: SonarBulkAnalyzerConfig) {
  private val log = Logger(this.getClass)

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
  def enhance(id: String, f: File): IO[Unit] = {
    gradleBuildFile(f)
      .use { lines =>
        IO(lines.getLines().toList)
      }
      .flatMap(lines => IO.pure(scriptWithPluginNode(lines)))
      .flatMap { script =>
        gradleBuildFileOut(f)
          .use {
            stream =>
              IO(stream.write(script.mkString("\n").getBytes(StandardCharsets.UTF_8)))
          }
      } *>
    IO(log.info(s"Finished adding plugin information to build script of $id"))
  }

  private def gradleBuildFile(f: File): Resource[IO, BufferedSource] = {
    val path = f.toPath.resolve("build.gradle")
    val input = IO {
      Source.fromFile(path.toFile)
    }

    Resource.fromAutoCloseable(input)
  }

  private def gradleBuildFileOut(f: File): Resource[IO, FileOutputStream] = {
    val path = f.toPath.resolve("build.gradle")
    val input = IO {
      new FileOutputStream(path.toFile, false)
    }

    Resource.fromAutoCloseable(input)
  }

  private def indexOfClosingParen(remainder: List[String], start: Int): Int = {
    @tailrec
    def processLine(line: List[Char], parens: Int, currLine: Int): (Int, Option[Int]) = {
      line match {
        case x :: xs =>
          x match {
            case '{' => processLine(xs, parens + 1, currLine)
            case '}' =>
              if (parens == 1) {
                (parens, currLine.some)
              } else {
                processLine(xs, parens - 1, currLine)
              }
            case _ => processLine(xs, parens, currLine)
          }
        case Nil => (parens, none)
      }
    }

    @tailrec
    def _indexOfClosingParen(lines: List[String], start: Int, parens: Int): Int = {
      lines match {
        case x :: xs =>
          processLine(x.toList, parens, start) match {
            case (_, Some(v)) => v
            case (p, _) => _indexOfClosingParen(xs, start + 1, p)
          }
        case Nil => -1
      }
    }

    _indexOfClosingParen(remainder, start, 1)
  }

  private def scriptWithPluginNode(lines: List[String]): List[String] = {
    val alreadyAdded = lines.exists(_.contains("id 'org.sonarqube' version"))
    if (alreadyAdded) {
      log.info("Build script already contains sonar plugin, skipping addition")
      lines
    } else {
      val hasBuildScript = lines.find(l => l.contains("buildscript {") || l.contains("buildscript{"))
      hasBuildScript match {
        case None => linesToInsert ++ lines
        case Some(v) =>
          val idx = lines.indexOf(v)
          val remainder = lines.drop(idx + 1)
          val closing = indexOfClosingParen(remainder, idx)

          lines.slice(0, closing + 2) ++ linesToInsert ++ lines.slice(closing + 2, lines.size)
      }
    }
  }
}
