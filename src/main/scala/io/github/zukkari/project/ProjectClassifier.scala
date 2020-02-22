package io.github.zukkari.project

import java.io.{File, FileFilter}

import cats.effect.{ContextShift, IO}
import cats.implicits._
import com.typesafe.scalalogging.Logger
import io.github.zukkari.SonarBulkAnalyzerConfig
import io.github.zukkari.execution._
import io.github.zukkari.git.GitRepository

import scala.annotation.tailrec

sealed trait ProjectKind {
  def buildFile: File
}

case class MavenProject(buildFile: File) extends ProjectKind

case class GradleProject(buildFile: File) extends ProjectKind

case object Unknown extends ProjectKind {
  override def buildFile: File = new File(".")
}

case class UnknownProject(msg: String) extends Exception

class ProjectClassifier(val config: SonarBulkAnalyzerConfig) {
  private val log = Logger(this.getClass)
  private implicit val contextShift: ContextShift[IO] = IO.contextShift(context)

  def classify(repos: List[GitRepository]): IO[List[ProjectBuilderKind]] = repos.map(classify).parSequence

  def classify(repo: GitRepository): IO[ProjectBuilderKind] = {
    classifyProject(repo.dir).flatMap {
      case GradleProject(f) =>
        IO(log.info(s"Project '$repo' classified as Gradle project")) *>
        IO.pure(new GradleProjectBuilderKind(repo.id, f, config))
      case MavenProject(f) =>
        IO(log.info(s"Project '$repo' classified as Maven project")) *>
        IO.pure(new MavenProjectBuilderKind(repo.id, f, config))
      case _ => IO(log.error(s"Project '$repo' does not use proper build system so we cannot analyze it")) *> IO.pure(NoOp)
    }
  }

  def classifyProject(dir: File): IO[ProjectKind] = {
    @tailrec
    def _classify(pending: List[File]): ProjectKind = {
      pending match {
        case x :: xs =>
          val children = x.listFiles(fileFilter)
          val kind = projectKind(x, children)
          kind match {
            case Some(k) => k
            case _ => _classify(xs ++ children)
          }
        case _ => Unknown
      }
    }

    IO {
      _classify(List(dir))
    }
  }

  def fileFilter: FileFilter = f => f.isDirectory || maven(f) || gradle(f)

  def gradle: File => Boolean = _.getName == "gradlew"

  def isSimpleGradle: File => Boolean = _.getName == "build.gradle"

  def maven: File => Boolean = _.getName == "pom.xml"

  def projectKind(parent: File, array: Array[File]): Option[ProjectKind] = {
    (array.exists(maven), array.exists(gradle) && array.exists(isSimpleGradle)) match {
      case (true, _) => MavenProject(parent).some
      case (_, true) => GradleProject(parent).some
      case _ => none
    }
  }
}
