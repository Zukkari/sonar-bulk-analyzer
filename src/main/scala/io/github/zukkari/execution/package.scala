package io.github.zukkari

import java.util.concurrent.{ExecutorService, Executors}

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

package object execution {
  val executor: ExecutorService = Executors.newFixedThreadPool(2)

  implicit val context: ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)
}
