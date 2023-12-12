package day4

import scala.io.Source
import scala.util.{Try, Using}

object Day4 {
  def calculateWinningCardPoints(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      points = lines.map(toPoints)
    } yield points.sum
  }

  def calculateTotalProcessedCards(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      count = lines.zipWithIndex.flatMap { case (line, index) =>
        toMatchesCount(line, index, lines)
      }
    } yield count.size
  }

  private def toPoints(line: String): Int = {
    val splittedNumbers = line.split(":")(1).split("\\|")
    val winningNumbers = splittedNumbers.head.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList
    val myNumbers = splittedNumbers.last.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList

    val overlappingNumbers = myNumbers.filter(number => winningNumbers.contains(number))

    Math.pow(2, overlappingNumbers.size - 1).toInt
  }

  private def toMatchesCount(line: String, lineIndex: Int, lines: List[String]): List[Int] = {
    val splittedNumbers = line.split(":")(1).split("\\|")
    val winningNumbers = splittedNumbers.head.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList
    val myNumbers = splittedNumbers.last.split("\\s+").filter(_.nonEmpty).map(_.toInt).toList

    val overlappingNumbersCount = myNumbers.count(number => winningNumbers.contains(number))

    val cardCopiesMatches = List.fill(overlappingNumbersCount)(()).zipWithIndex.flatMap { case (_, index) =>
      val nextIndex = lineIndex + index + 1
      val nextLine = lines(nextIndex)

      toMatchesCount(nextLine, nextIndex, lines)
    }
    cardCopiesMatches.appended(1)
  }

}

object Main extends App {
//  private val example1 = Day4.calculateWinningCardPoints("./src/main/scala/day4/example.txt")
//  private val example2 = Day4.calculateTotalProcessedCards("./src/main/scala/day4/example.txt")
//  private val value1 = Day4.calculateWinningCardPoints("./src/main/scala/day4/input.txt")
  private val value2 = Day4.calculateTotalProcessedCards("./src/main/scala/day4/input.txt")
  println(value2)
}
