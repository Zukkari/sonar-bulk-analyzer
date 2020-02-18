package io.github.zukkari.project

import java.io.{File, FileOutputStream, OutputStream}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import cats.effect.{IO, Resource}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig


case class BuildFailedException(msg: String) extends Exception

abstract class ProjectBuilderKind {
  private val log = Logger(this.getClass)

  def id: String

  def project: File

  def build(outFile: IO[File]): IO[ProjectBuilderKind] = {
    for {
      out <- outFile
      wrapper <- usesWrapper
      project <- run(wrapper, out)
    } yield project
  }

  def wrapperName: String

  def executableName: String

  def usesWrapper: IO[Boolean] =
    IO {
      project.list((_, name) => wrapperName contains name).length > 0
    }

  def buildArgs: List[String]

  private def permissionAction: IO[Unit] = IO {
    val process = new ProcessBuilder()
      .directory(project)
      .command("chmod", "+x", wrapperName)
      .inheritIO()
      .start()

    val exited = process.waitFor(5L, TimeUnit.MINUTES)
    if (exited) process.exitValue() else {
      log.warn(s"Process for project $project timed out, destroying")
      val destructionProcess = process.destroyForcibly()
      destructionProcess.waitFor()
      log.info(s"Process for project $project destroyed successfully")
      1
    }
  }.flatMap {
    case 0 => IO(log.info(s"Successfully gave execute permission for wrapper in project: $project"))
    case errCode => IO.raiseError(BuildFailedException(s"Failed to give permission to the project: '$project' due to exit code: '$errCode'"))
  }

  def run(useWrapper: Boolean, outputStream: File): IO[ProjectBuilderKind] = {
    val giveExecutePermission = if (useWrapper) {
      // If we use wrapper we have to give execute permission
      // its always a good idea to run random executables from the internet
      permissionAction
    } else {
      IO.unit
    }

    val preProcess = for {
      f <- createPropertiesFile
      sdkLocation <- IO(System.getenv("ANDROID_HOME"))
      _ <- writeSdkLocation(f, sdkLocation)
    } yield ()

    preProcess *>
      giveExecutePermission *>
      IO {
        val executable = if (useWrapper) wrapperName else executableName
        val command = executable :: buildArgs
        log.info(s"Building project with command: '$command'")
        processBuilder(outputStream, command)
      }.flatMap {
        case 0 => IO {
          log.info(s"Build for $id finished with SUCCESS")
          this
        }
        case exitCode => IO {
          log.error(s"Build for $id finished with ERROR: $exitCode. Details: ${outputStream.getAbsolutePath}")
          NoOp
        }
      }
  }

  def createPropertiesFile: IO[File] =
    IO {
      val localPropsFile = project.toPath.resolve("local.properties").toFile
      localPropsFile.delete()
      localPropsFile.createNewFile()
      localPropsFile
    }

  def writeSdkLocation(f: File, sdkLocation: String): IO[Unit] = {
    mkLocalPropsResource(f)
      .use { stream =>
        val bytes = s"sdk.location=$sdkLocation".getBytes(StandardCharsets.UTF_8)
        IO(stream.write(bytes))
      }
  }

  def mkLocalPropsResource(f: File): Resource[IO, OutputStream] = {
    val stream = IO {
      new FileOutputStream(f)
    }

    Resource.fromAutoCloseable(stream)
  }

  def analysisArgs: List[String]

  def runAnalysis(logFile: IO[File]): IO[Unit] = {
    (for {
      f <- logFile
      useWrapper <- usesWrapper
      exitCode <- IO {
        val executable = if (useWrapper) wrapperName else executableName
        val command = executable :: analysisArgs
        log.info(s"Running analysis with command: '$command'")

        processBuilder(f, command)
      }
    } yield exitCode)
      .flatMap {
        case 0 => IO {
          log.info(s"Analysis for $id finished with SUCCESS")
        }
        case exitCode => IO {
          log.error(s"Analysis for $id finished with ERROR: $exitCode.")
        }
      } *>
      IO {
        log.info(s"Analysis finished for project: $id")
      }
  }

  private def processBuilder(f: File, command: List[String]) = {
    val process = new ProcessBuilder()
      .directory(project)
      .command(command: _*)
      .redirectOutput(f)
      .redirectError(f)
      .start()

    val exited = process.waitFor(5L, TimeUnit.MINUTES)
    if (exited) process.exitValue() else {
      log.warn(s"Process for project $project timed out, destroying")
      val destructionProcess = process.destroyForcibly()
      destructionProcess.waitFor()
      log.info(s"Process for project $project destroyed successfully")
      1
    }
  }
}

class MavenProjectBuilderKind(val id: String, val project: File, val config: SonarBulkAnalyzerConfig) extends ProjectBuilderKind {

  override def wrapperName: String = "./mvnw"

  override def buildArgs: List[String] = List("package", "-DskipTests")

  override def executableName: String = "mvn"

  override def analysisArgs: List[String] = List("sonar:sonar",
    s"-Dsonar.projectKey=$id",
    s"-Dsonar.host.url=${config.sonarUrl}",
    s"-Dsonar.login=${config.sonarToken}"
  )
}

class GradleProjectBuilderKind(val id: String, val project: File, val config: SonarBulkAnalyzerConfig) extends ProjectBuilderKind {

  override def wrapperName: String = "./gradlew"

  override def buildArgs: List[String] = List("build", "-x", "test")

  override def executableName: String = "gradle"

  override def analysisArgs: List[String] = List(
    "-x",
    "test",
    "sonarqube",
    s"-Dsonar.projectKey=$id",
    s"-Dsonar.host.url=${config.sonarUrl}",
    s"-Dsonar.login=${config.sonarToken}"
  )
}

case object NoOp extends ProjectBuilderKind {
  override def project: File = ???

  override def wrapperName: String = ???

  override def buildArgs: List[String] = ???

  override def executableName: String = ???

  override def id: String = ???

  override def analysisArgs: List[String] = ???
}
