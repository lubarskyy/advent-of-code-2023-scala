package day1

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

object Day1 {
  private val stringDigitsMap = Map(
    "one" -> "1",
    "two" -> "2",
    "three" -> "3",
    "four" -> "4",
    "five" -> "5",
    "six" -> "6",
    "seven" -> "7",
    "eight" -> "8",
    "nine" -> "9"
  )
  private val stringDigitsPattern2 = "(?=(one|two|three|four|five|six|seven|eight|nine|\\d))".r

  def extractCalibrationValue(path: String): Try[Long] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      digits = lines.map(extractDigitsFromLine)
    } yield digits.sum
  }

  private def extractDigitsFromLine(line: String): Long = {
    val mappedDigitsLine = stringDigitsPattern2
      .findAllMatchIn(line)
      .map(_.group(1))
      .map((maybeDigit) => (maybeDigit, Try(maybeDigit.toInt)))
      .map {
        case (maybeDigit, Success(_)) => Some(maybeDigit)
        case (maybeDigit, Failure(_)) => stringDigitsMap.get(maybeDigit)
      }
      .collect { case Some(value) =>
        value
      }
      .toList

    val result = mappedDigitsLine.head + mappedDigitsLine.last

    result.toLong
  }
}

object Main extends App {
  private val value = Day1.extractCalibrationValue("./src/main/scala/day1/input.txt")
  println(value)
}
