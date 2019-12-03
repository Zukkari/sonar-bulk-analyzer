package io.github.zukkari.actors

import akka.actor.{Actor, Props, Terminated}
import akka.routing.{ActorRefRoutee, Router, SmallestMailboxRoutingLogic}
import io.github.zukkari.git.GitSource
import io.github.zukkari.parser.Project

class GitSourceRouter extends Actor {
  private val router = {
    val routees = Vector.fill(10) {
      val r = context.actorOf(Props[GitSource])
      context.watch(r)
      ActorRefRoutee(r)
    }

    Router(SmallestMailboxRoutingLogic(), routees)
  }

  override def receive: Receive = onMessage(router)

  private def onMessage(router: Router): Receive = {
    case p: Project => router.route(p, sender())
    case Terminated(a) =>
      context.become(onMessage(router.removeRoutee(a)))
      val r = context.actorOf(Props[GitSource])
      context.watch(r)
      context.become(onMessage(router.addRoutee(r)))
  }
}
