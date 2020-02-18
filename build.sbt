name := "sonar-bulk-analyzer"

version := "0.1"

scalaVersion := "2.13.1"

scalacOptions := Seq(
  "-encoding", "utf8",
  "-Xfatal-warnings",
  "-deprecation",
  "-language:higherKinds",
)

val http4sVersion = "0.21.0-M6"

// Dependencies
val sonarVersion = "7.9"
libraryDependencies ++= Seq(
  "org.scala-lang" % "scala-library" % "2.13.0",
  "org.typelevel" %% "cats-core" % "2.0.0",
  "org.typelevel" %% "cats-effect" % "2.0.0",
  "com.typesafe.scala-logging" %% "scala-logging" % "3.9.2",
  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "com.github.scopt" %% "scopt" % "4.0.0-RC2",
  "org.eclipse.jgit" % "org.eclipse.jgit" % "5.5.1.201910021850-r",
  "org.http4s" %% "http4s-dsl" % http4sVersion,
  "org.http4s" %% "http4s-blaze-client" % http4sVersion,
  "org.scalatest" %% "scalatest" % "3.2.0-M1" % Test,
  "org.scalatestplus" %% "scalatestplus-mockito" % "1.0.0-M2" % Test,
)

libraryDependencies += "io.circe" %% "circe-core" % "0.13.0"
libraryDependencies += "io.circe" %% "circe-parser" % "0.13.0"

def isSignatureFile(f: String): Boolean = {
  f.endsWith("DSA") ||
  f.endsWith("SF") ||
  f.endsWith("RSA")
}

// Assembly
test in assembly := {}
assemblyJarName in assembly := s"${name.value}-${version.value}.jar"
mainClass in assembly := Some("io.github.zukkari.SonarBulkAnalyzer")
assemblyMergeStrategy in assembly := {
  case "log4j.properties" => MergeStrategy.first
  case "reference.conf" => MergeStrategy.concat
  case "application.conf" => MergeStrategy.concat
  case signed if isSignatureFile(signed)=> MergeStrategy.discard
  case PathList("META-INF", xs@_*) =>
    xs match {
      case "MANIFEST.MF" :: Nil => MergeStrategy.discard
      case _ => MergeStrategy.first
    }
  case _ => MergeStrategy.first
}
artifact in(Compile, assembly) := {
  val art = (artifact in(Compile, assembly)).value
  art.withClassifier(Some("assembly"))
}
addArtifact(artifact in(Compile, assembly), assembly)
