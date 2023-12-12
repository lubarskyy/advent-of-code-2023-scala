package day2

import scala.io.Source
import scala.util.{Try, Using}

object Day2 {
  private val maximumRedCubes = 12
  private val maximumGreenCubes = 13
  private val maximumBlueCubes = 14

  private val redCubesPattern = "([0-9]+) red".r
  private val greenCubesPattern = "([0-9]+) green".r
  private val blueCubesPattern = "([0-9]+) blue".r
  private val gameIdPattern = "([0-9]+):".r

  def calculateGameIdsSum(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      possibleGamesLines = lines.filter(possibleGames)
      ids = possibleGamesLines.map(extractGameId)
    } yield ids.sum
  }

  def calculateGameSetPowerSum(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      setPower = lines.map(toSetPower)
    } yield setPower.sum
  }

  private def possibleGames(line: String): Boolean = {
    val reds = redCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList
    val greens = greenCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList
    val blues = blueCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList

    !(reds.exists(_ > maximumRedCubes) || greens.exists(_ > maximumGreenCubes) || blues.exists(_ > maximumBlueCubes))
  }

  private def extractGameId(line: String): Int = {
    gameIdPattern.findAllMatchIn(line).map(_.group(1)).map(_.toInt).toList.head
  }

  private def toSetPower(line: String): Int = {
    val reds = redCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList
    val greens = greenCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList
    val blues = blueCubesPattern.findAllMatchIn(line).map(_.group(1).toInt).toList

    reds.max * greens.max * blues.max
  }
}

object Main extends App {
  private val value1 = Day2.calculateGameIdsSum("./src/main/scala/day2/input.txt")
  private val value2 = Day2.calculateGameSetPowerSum("./src/main/scala/day2/input.txt")
  println(value2)
}
