package io.github.zukkari.parser

import java.io.File

class FDroidProjectFileParserImplTest extends TestSpec {
  val parser = new FDroidProjectFileParserImpl

  it should "create a resource from file" in {
    val f = mock[File]

    val resource = parser.mkResource(f)
    assert(resource != null)
  }

  it should "parse line into IO[Project]" in {
    val project = parser.mkProject("| 1. project | description | url |").unsafeRunSync()

    assert(project.name == "project")
    assert(project.description == "description")
    assert(project.url == "url")
  }

  it should "catch return an error for lines that it cannot parse" in {
    val project = parser.mkProject("very bad line")

    project.attempt
      .unsafeRunSync() match {
      case Left(_) => succeed
      case Right(_) => fail("How the hell you parsed this?")
    }
  }
}
