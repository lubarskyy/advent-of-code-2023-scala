package day5

import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.matching.Regex
import scala.util.{Try, Using}
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

object Day5 {
  private val seedsPattern = "seeds: (.*?)(?:\\r?\\n\\r?\\n|$)".r
  private val seedToSoilPattern = "(?s)seed-to-soil map:(.*?)(?:\\r*\\n){2}".r
  private val soilToFertilizerPattern = "(?s)soil-to-fertilizer map:(.*?)(?:\\r*\\n){2}".r
  private val fertilizerToWaterPattern = "(?s)fertilizer-to-water map:(.*?)(?:\\r*\\n){2}".r
  private val waterToLightPattern = "(?s)water-to-light map:(.*?)(?:\\r*\\n){2}".r
  private val lightToTemperaturePattern = "(?s)light-to-temperature map:(.*?)(?:\\r*\\n){2}".r
  private val temperatureToHumidityPattern = "(?s)temperature-to-humidity map:(.*?)(?:\\r*\\n){2}".r
  private val humidityToLocationPattern = "(?s)humidity-to-location map:(.*?)(?:(?:\\r*\\n){2}|$)".r

  def findLowestLocationValue(path: String): Try[Long] = {
    for {
      file <- Using(Source.fromFile(path))(file => file.mkString)
      seeds = parseFile(file, seedsPattern)
      seedToSoilRanges = toRanges(parseFile(file, seedToSoilPattern))
      soilToFertilizerRanges = toRanges(parseFile(file, soilToFertilizerPattern))
      fertilizerToWaterRanges = toRanges(parseFile(file, fertilizerToWaterPattern))
      waterToLightRanges = toRanges(parseFile(file, waterToLightPattern))
      lightToTemperatureRanges = toRanges(parseFile(file, lightToTemperaturePattern))
      temperatureToHumidityRanges = toRanges(parseFile(file, temperatureToHumidityPattern))
      humidityToLocationRanges = toRanges(parseFile(file, humidityToLocationPattern))
      locations = seeds.map(seed =>
        determineLocation(
          seed,
          seedToSoilRanges,
          soilToFertilizerRanges,
          fertilizerToWaterRanges,
          waterToLightRanges,
          lightToTemperatureRanges,
          temperatureToHumidityRanges,
          humidityToLocationRanges
        )
      )
      _ = println(locations)
    } yield locations.min
  }

  def findLowestLocationValueFromSeedRange(path: String): Try[Long] = {
    for {
      file <- Using(Source.fromFile(path))(file => file.mkString)
      seeds = parseFile(file, seedsPattern)
      seedToSoilRanges = toRanges(parseFile(file, seedToSoilPattern))
      soilToFertilizerRanges = toRanges(parseFile(file, soilToFertilizerPattern))
      fertilizerToWaterRanges = toRanges(parseFile(file, fertilizerToWaterPattern))
      waterToLightRanges = toRanges(parseFile(file, waterToLightPattern))
      lightToTemperatureRanges = toRanges(parseFile(file, lightToTemperaturePattern))
      temperatureToHumidityRanges = toRanges(parseFile(file, temperatureToHumidityPattern))
      humidityToLocationRanges = toRanges(parseFile(file, humidityToLocationPattern))
      locationFutures = seeds
        .grouped(2)
        .toList
        .map(seed => {
          Future {
            val seedRangeStart = seed.head
            val seedRangeLength = seed.last
            val seedRange = seedRangeStart until (seedRangeStart + seedRangeLength)

            seedRange.map(seed =>
              determineLocation(
                seed,
                seedToSoilRanges,
                soilToFertilizerRanges,
                fertilizerToWaterRanges,
                waterToLightRanges,
                lightToTemperatureRanges,
                temperatureToHumidityRanges,
                humidityToLocationRanges
              )
            )
          }
        })
      locations = Await.result(Future.sequence(locationFutures), Duration.Inf)
      _ = println(locations)
    } yield locations.flatten.min
  }

  private def parseLineOfNumbers(line: String): List[Long] = {
    line.split("\\s+").filter(_.nonEmpty).map(_.toLong).toList
  }

  private def parseFile(file: String, pattern: Regex): List[Long] = {
    pattern.findAllMatchIn(file).flatMap(matched => parseLineOfNumbers(matched.group(1))).toList
  }

  private def toRanges(list: List[Long]): List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])] = {
    list
      .grouped(3)
      .map { case List(destinationRangeStart, sourceRangeStart, rangeLength) =>
        (
          (destinationRangeStart until destinationRangeStart + rangeLength) ->
            (sourceRangeStart until sourceRangeStart + rangeLength)
        )
      }
      .toList
  }

  private def isInRange(
      input: Long,
      ranges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])]
  ): Option[Long] = {
    ranges.collectFirst {
      case (destinationRange, sourceRange) if sourceRange.contains(input) => {
        (input - sourceRange.start) + destinationRange.start
      }
    }
  }

//  private def isRangeOverlapping(range: NumericRange.Exclusive[Long], ranges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])]): Option[Long] = {
  //    ranges.collectFirst {
  //      case (destinationRange, sourceRange) if range.start
  //      (input - sourceRange.start) + destinationRange.start
  //    }
  //  }
//}

  private def determineLocation(
      seed: Long,
      seedToSoilRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      soilToFertilizerRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      fertilizerToWaterRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      waterToLightRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      lightToTemperatureRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      temperatureToHumidityRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])],
      humidityToLocationRanges: List[(NumericRange.Exclusive[Long], NumericRange.Exclusive[Long])]
  ): Long = {
    val soil = isInRange(seed, seedToSoilRanges).getOrElse(seed)
    val fertilizer = isInRange(soil, soilToFertilizerRanges).getOrElse(soil)
    val water = isInRange(fertilizer, fertilizerToWaterRanges).getOrElse(fertilizer)
    val light = isInRange(water, waterToLightRanges).getOrElse(water)
    val temperature = isInRange(light, lightToTemperatureRanges).getOrElse(light)
    val humidity = isInRange(temperature, temperatureToHumidityRanges).getOrElse(temperature)
    val location = isInRange(humidity, humidityToLocationRanges).getOrElse(humidity)

    location
  }
}

object Main extends App {
  //    private val example1 = Day5.findLowestLocationValue("./src/main/scala/day5/example.txt")
  //    private val example2 = Day5.findLowestLocationValueFromSeedRange("./src/main/scala/day5/example.txt")
  //    private val value1 = Day5.findLowestLocationValue("./src/main/scala/day5/input.txt")
  private val value2 = Day5.findLowestLocationValueFromSeedRange("./src/main/scala/day5/input.txt")
  println(value2)
}
