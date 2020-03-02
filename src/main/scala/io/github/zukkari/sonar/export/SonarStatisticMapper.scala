package io.github.zukkari.sonar.`export`

import cats.data.Reader
import com.typesafe.scalalogging.Logger
import io.circe.{Json, JsonNumber}
import io.circe.parser._

case class InterfaceStatistic(numberOfMethods: Int)

case class ClassStatistics(attributes: Int,
                           methods: Int,
                           instructions: Int,
                           comments: Int,
                           complexity: Int,
                           complexityRatio: Double,
                           coupling: Int,
                           cohesion: Int)

case class MethodStatistics(complexity: Int,
                            calls: Int,
                            instructions: Int,
                            parameters: Int,
                            chainLength: Int,
                            switchStatements: Int)

class SonarStatisticMapper {
  def map(issues: List[SonarIssue]): List[String] = {
    issues
      .map(transform)
      .reduce(_ ++ _)
  }

  def issueTypeReader: Reader[Json, String] =
    Reader { json =>
      json.hcursor
        .downField("issueType")
        .focus
        .flatMap(_.asString)
        .getOrElse("")
    }

  private def parseClassRule(message: String): List[String] = {
    parse(message) match {
      case Left(parsingFailure) => throw parsingFailure
      case Right(json) =>
        val issueType = issueTypeReader.run(json)

        if (issueType == "class") {
          val stats = classStatisticReader.run(json)
          List(
            s"class;attributes;${stats.attributes}",
            s"class;methods;${stats.methods}",
            s"class;instructions;${stats.instructions}",
            s"class;comments;${stats.comments}",
            s"class;complexity;${stats.complexity}",
            s"class;complexityRatio;${stats.complexityRatio}",
            s"class;coupling;${stats.coupling}",
            s"class;cohesion;${stats.cohesion}",
          )
        } else {
          val method = methodStatisticsReader.run(json)
          List(
            s"method;complexity;${method.complexity}",
            s"method;calls;${method.calls}",
            s"method;instructions;${method.instructions}",
            s"method;parameters;${method.parameters}",
            s"method;chainLength;${method.chainLength}",
            s"method;switchStatements;${method.switchStatements}",
          )
        }
    }
  }

  private def parseInterfaceRule(message: String): List[String] =
    parse(message) match {
      case Left(parsingFailure) =>
        throw parsingFailure
      case Right(json) => parseInterfaceJson(json)
    }

  def parseInterfaceJson(json: Json): List[String] = {
    val statistic = interfaceStatisticReader.run(json)
    List(s"interface;numberOfMethods;${statistic.numberOfMethods}")
  }

  private def transform(issue: SonarIssue): List[String] = {
    if (issue.rule.contains("ClassStatsCollector")) {
      parseClassRule(issue.message)
    } else if (issue.rule.contains("InterfaceStatsCollector")) {
      parseInterfaceRule(issue.message)
    } else {
      Nil
    }
  }

  private def interfaceStatisticReader: Reader[Json, InterfaceStatistic] =
    Reader { json =>
      json.hcursor
        .downField("numberOfMethods")
        .focus
        .flatMap(_.asNumber)
        .flatMap(_.toInt)
        .map(InterfaceStatistic)
        .getOrElse(InterfaceStatistic(0))
    }

  private def classStatisticReader: Reader[Json, ClassStatistics] =
    Reader { json =>
      val attributes = intFieldReader("attributes").run(json)
      val methods = intFieldReader("methods").run(json)
      val instructions = intFieldReader("instructions").run(json)
      val comments = intFieldReader("comments").run(json)
      val complexity = intFieldReader("complexity").run(json)
      val complexityRatio = doubleFieldReader("complexityRatio").run(json)
      val coupling = intFieldReader("coupling").run(json)
      val cohesion = intFieldReader("cohesion").run(json)

      val methodReader = methodStatisticsReader
      val methodList = json.hcursor
        .downField("methodList")
        .focus
        .flatMap(_.asArray)
        .getOrElse(Vector.empty)
        .map(methodReader.run)
        .toList

      ClassStatistics(
        attributes,
        methods,
        instructions,
        comments,
        complexity,
        complexityRatio,
        coupling,
        cohesion
      )
    }

  private def methodStatisticsReader: Reader[Json, MethodStatistics] =
    Reader { json =>
      val complexity = intFieldReader("complexity").run(json)
      val calls = intFieldReader("calls").run(json)
      val instructions = intFieldReader("instructions").run(json)
      val parameters = intFieldReader("parameters").run(json)
      val chainLength = intFieldReader("chainLength").run(json)
      val switchStatements = intFieldReader("switchStatements").run(json)

      MethodStatistics(
        complexity,
        calls,
        instructions,
        parameters,
        chainLength,
        switchStatements
      )
    }

  private def intFieldReader(name: String): Reader[Json, Int] =
    fieldReader(name, _.toInt).map(_.getOrElse(0))

  private def doubleFieldReader(name: String): Reader[Json, Double] = {
    fieldReader(name, _.toBigDecimal.map(_.toDouble)).map(_.getOrElse(0.0))
  }

  private def fieldReader[T](
    name: String,
    mapper: JsonNumber => Option[T]
  ): Reader[Json, Option[T]] = {
    Reader { json =>
      json.hcursor
        .downField(name)
        .focus
        .flatMap(_.asNumber)
        .flatMap(mapper)
    }
  }
}
