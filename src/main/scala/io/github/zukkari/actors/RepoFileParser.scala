package io.github.zukkari.actors

import java.io.File

import akka.actor.{Actor, Props}
import akka.event.Logging
import io.github.zukkari.parser.ProjectFileParser

import scala.io.Source
import scala.util.Using

object RepoFileParser {
  def props(parser: ProjectFileParser): Props = Props(new RepoFileParser(parser))
}

class RepoFileParser(private val parser: ProjectFileParser) extends Actor {
  private val log = Logging(context.system, this)

  override def receive: Receive = {
    case repoFile: File =>
      log.info(s"Received file: ${repoFile.getAbsolutePath}")

      val router = context.actorSelection("../gitSourceRouter")

      Using(Source.fromFile(repoFile))(_.getLines().toSeq)
        .map(seq => seq.map(parser.parse))
        .fold(
          err => {
            log.error("Error when parsing input file", err)
            Seq.empty
          },
          identity
        )
        .foreach({
          case Some(project) => router ! project
          case _ =>
        })
    case _ =>
      log.error("Received unknown message", _)
  }

}

