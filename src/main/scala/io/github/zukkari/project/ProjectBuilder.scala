package io.github.zukkari.project

import java.io.File

import cats.effect.IO

abstract class ProjectBuilder {
  def project: File

  def build: IO[Unit]
}

class MavenProjectBuilder(val project: File) extends ProjectBuilder {
  override def build: IO[Unit] = ???
}

class GradleProjectBuilder(val project: File) extends ProjectBuilder {

  override def build: IO[Unit] = ???

}

case object NoOp extends ProjectBuilder {
  override def project: File = ???

  override def build: IO[Unit] = IO.unit
}
