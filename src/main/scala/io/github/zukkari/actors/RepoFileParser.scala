package io.github.zukkari.actors

import java.io.File

import akka.actor.{Actor, ActorSelection, Props}
import akka.event.Logging
import cats.effect.{IO, Resource}
import io.github.zukkari.parser.{Project, ProjectFileParser}

import scala.annotation.tailrec
import scala.io.Source

object RepoFileParser {
  def props(parser: ProjectFileParser): Props = Props(new RepoFileParser(parser))
}

class RepoFileParser(private val parser: ProjectFileParser) extends Actor {
  private val log = Logging(context.system, this)

  override def receive: Receive = {
    case repoFile: File =>
      log.info(s"Received file: ${repoFile.getAbsolutePath}")

      implicit val router: ActorSelection = context.actorSelection("../gitSourceRouter")
      parse(repoFile)
        .runAsync {
          case Left(err) => IO {
            log.error("Error when parsing input file", err)
          }
          case Right(c) => IO {
            log.info(s"Successfully parsed $c projects")
          }
        }
    case _ =>
      log.error("Received unknown message", _)
  }

  def parse(f: File)(implicit router: ActorSelection): IO[Int] =
    lines(f)
      .map(transform)
      .use(send)

  def send(seq: Seq[Project])(implicit router: ActorSelection): IO[Int] = {
    @tailrec
    def _send(seq: Seq[Project], acc: Int): IO[Int] = {
      seq match {
        case x :: xs =>
          router ! x
          _send(xs, acc + 1)
        case Nil =>
          IO.pure(acc)
      }
    }

    _send(seq, 0)
  }

  def transform(seq: Seq[String]): Seq[Project] =
    for {
      line <- seq
      parsed <- parser.parse(line)
    } yield parsed


  def lines(f: File): Resource[IO, Seq[String]] = {
    val io = IO {
      Source.fromFile(f)
    }

    Resource.fromAutoCloseable(io)
      .map(_.getLines().toSeq)
  }
}

