package io.github.zukkari.sonar.`export`

import java.io.PrintWriter
import java.nio.file.{FileAlreadyExistsException, Files, Path}

import cats.effect.{IO, Resource}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig

class SonarIssueExporter(val config: SonarBulkAnalyzerConfig) {
  private val log = Logger(classOf[SonarIssueExporter])

  def export(sonarIssues: List[SonarIssue]): IO[Unit] = {
    val allIssues = sonarIssues.map(_.rule).distinct
      .filter(_.startsWith(config.rulePrefix))
    val issuesByProject = sonarIssues.groupBy(_.project)

    IO(log.info(s"Exporting to file ${config.`export`}")).flatMap { _ =>
      val f = config.`export`
      if (f.exists()) {
        log.error(s"${config.`export`} already exists")
        IO.raiseError(new FileAlreadyExistsException(f.getAbsolutePath))
      } else {
        log.info(s"Creating export file at ${config.`export`}")
        IO(Files.createFile(f.toPath))
      }
    }.flatMap { path =>
      makeExportResource(path).use { writer =>
        IO {
          writer.println("project;" ++ allIssues.mkString(";"))
          issuesByProject.foreachEntry {
            case (project, issues) =>
              val grouped = issues.groupBy(_.rule)
              val issueString = allIssues.map(issue => grouped.get(issue).map(_.size).getOrElse(0)).mkString(";")
              writer.println(project ++ ";" ++ issueString)
          }
        }
      }
    } *>
    IO {
      log.info("Finished writing results to file")
    }
  }

  def makeExportResource(path: Path): Resource[IO, PrintWriter]  = {
    val aquire = IO {
      new PrintWriter(path.toString, "UTF-8")
    }

    Resource.make(aquire) {
      writer =>
        IO {
          writer.flush()
          log.info("Flushed writer")
          writer.close()
          log.info("Closed writer")
        }
    }
  }
}
