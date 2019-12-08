package io.github.zukkari.project

import java.io.File

import cats.effect.IO
import cats.implicits._
import com.typesafe.scalalogging.Logger

case class BuildFailedException(msg: String) extends Exception

abstract class ProjectBuilderKind {
  private val log = Logger(this.getClass)

  def project: File

  def build: IO[Unit] = {
    usesWrapper.flatMap(run)
  }

  def wrapperName: String

  def executableName: String

  def usesWrapper: IO[Boolean] =
    IO {
      project.list((_, name) => wrapperName contains name).length > 0
    }

  def args: List[String]

  private def permissionAction: IO[Unit] = IO {
    new ProcessBuilder()
      .directory(project)
      .command("chmod", "+x", wrapperName)
      .inheritIO()
      .start()
      .waitFor()
  }.flatMap {
    case 0 => IO(log.info(s"Successfully gave execute permission for wrapper in project: $project"))
    case errCode => IO.raiseError(BuildFailedException(s"Failed to give permission to the project: '$project' due to exit code: '$errCode'"))
  }

  def run(useWrapper: Boolean): IO[Unit] = {
    val execPermission = if (useWrapper) {
      // If we use wrapper we have to give execute permission
      // its always a good idea to run random executables from the internet
      permissionAction
    } else {
      IO.unit
    }

    execPermission *>
      IO {
        val executable = if (useWrapper) wrapperName else executableName
        val command = executable :: args
        log.info(s"Building project with command: '$command'")

        new ProcessBuilder()
          .directory(project)
          .command(command: _*)
          .inheritIO()
          .start()
          .waitFor()
      }.flatMap {
        case 0 => IO.unit
        case exitCode => IO.raiseError(BuildFailedException(s"Build failed with exit code: $exitCode"))
      }
  }
}

class MavenProjectBuilderKind(val project: File) extends ProjectBuilderKind {

  override def wrapperName: String = "./mvnw"

  override def args: List[String] = List("package", "-DskipTests")

  override def executableName: String = "mvn"
}

class GradleProjectBuilderKind(val project: File) extends ProjectBuilderKind {

  override def wrapperName: String = "./gradlew"

  override def args: List[String] = List("build", "-x", "test")

  override def executableName: String = "gradle"
}

case object NoOp extends ProjectBuilderKind {
  override def project: File = ???

  override def wrapperName: String = ???

  override def args: List[String] = ???

  override def executableName: String = ???
}
