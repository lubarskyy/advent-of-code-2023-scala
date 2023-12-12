package day3

import scala.io.Source
import scala.util.{Try, Using}

object Day3 {
  private val numbersPattern = "([0-9]+)".r
  private val engineGearPattern = "\\*".r

  def calculateEnginePartNumbersSum(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      partNumberSumPerLine = lines.zipWithIndex.map { case (line, index) =>
        extractEnginePartNumber(line, lines.lift(index - 1), lines.lift(index + 1))
      }
    } yield partNumberSumPerLine.sum
  }

  def calculateEngineGearRatioSum(path: String): Try[Int] = {
    for {
      lines <- Using(Source.fromFile(path))(file => file.getLines().toList)
      numberRanges = buildNumberRanges(lines)
      gearRatioSumPerLine = lines.zipWithIndex.map { case (line, index) =>
        extractEngineGearRatio(line, index, numberRanges)
      }
    } yield gearRatioSumPerLine.sum
  }

  private def extractEnginePartNumber(middleLine: String, topLine: Option[String], bottomLine: Option[String]): Int = {
    val partNumbers = numbersPattern
      .findAllMatchIn(middleLine)
      .map(matched => {
        val number = matched.group(0).toInt
        val proximityIndexStart = (matched.start - 1).max(0)
        val proximityIndexEnd = matched.end.min(139) // we know max length of line is 140
        val proximityRange = proximityIndexStart to proximityIndexEnd

        val matchingSymbols = proximityRange.map(index => {
          val topChar = topLine.map(_(index) != '.')
          val middleCharLeft = Option.when(index == proximityIndexStart && index != 0)(middleLine(index) != '.')
          val middleCharRight = Option.when(index == proximityIndexEnd && index != 139)(middleLine(index) != '.')
          val bottomChar = bottomLine.map(_(index) != '.')

          topChar.contains(true) || middleCharLeft.contains(true) || middleCharRight.contains(true) || bottomChar
            .contains(true)
        })

        if (matchingSymbols.contains(true)) number else 0
      })

    partNumbers.toList.sum
  }

  private def extractEngineGearRatio(
      middleLine: String,
      lineIndex: Int,
      numberRanges: List[(Int, Int, Range)]
  ): Int = {
    val gearRatios = engineGearPattern
      .findAllMatchIn(middleLine)
      .map(matched => {
        // NOTE: I've skipped the check for out of bounds index access as input file does not have such scenarios
        val proximityIndexStart = matched.start - 1
        val proximityIndexEnd = matched.end
        val proximityRange = proximityIndexStart to proximityIndexEnd

        val proximityMatches = for {
          index <- proximityRange
          topChar = matchToNumberRange(index, lineIndex - 1, numberRanges)
          middleChar = matchToNumberRange(index, lineIndex, numberRanges)
          bottomChar = matchToNumberRange(index, lineIndex + 1, numberRanges)
        } yield (topChar, middleChar, bottomChar)

        val (top, middle, bottom) = proximityMatches.unzip3
        val topDistinctInts = top.flatten.distinct
        val middleDistinctInts = middle.flatten.distinct
        val bottomDistinctInts = bottom.flatten.distinct

        val distinctInts = topDistinctInts ++ middleDistinctInts ++ bottomDistinctInts

        if (distinctInts.sizeIs == 2) {
          distinctInts.product
        } else {
          0
        }
      })

    gearRatios.toList.sum
  }

  private def buildNumberRanges(lines: List[String]): List[(Int, Int, Range)] = {
    lines.zipWithIndex.flatMap { case (line, index) =>
      numbersPattern
        .findAllMatchIn(line)
        .map(matched => {
          val number = matched.group(0).toInt
          val range = matched.start until matched.end

          (index, number, range)
        })
        .toList
    }
  }

  private def matchToNumberRange(index: Int, lineIndex: Int, numberRanges: List[(Int, Int, Range)]): Option[Int] = {
    val numberRange = numberRanges.find(numberRange => (numberRange._1 == lineIndex) && numberRange._3.contains(index))

    numberRange.map(_._2)
  }

}

object Main extends App {
  private val value1 = Day3.calculateEnginePartNumbersSum("./src/main/scala/day3/input.txt")
  private val value2 = Day3.calculateEngineGearRatioSum("./src/main/scala/day3/input.txt")
  println(value2)
}
