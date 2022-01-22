package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day6 extends Exercise {

  private def parseInput: PopulationCounts = {
    val source = fromFile(path)

    val parsedSource = source.getLines().flatMap(_.split(",")).map(_.toInt).toList
    source.close()
    val valueCounts = parsedSource.groupMapReduce(identity)(_ => 1L)(_ + _).withDefaultValue(0L)
    PopulationCounts(valueCounts(0), valueCounts(1), valueCounts(2), valueCounts(3), valueCounts(4),
      valueCounts(5), valueCounts(6), valueCounts(7), valueCounts(8))
  }

  case class PopulationCounts(zeros: Long, ones: Long, twos: Long, threes: Long, fours: Long,
                              fives: Long, sixes: Long, sevens: Long, eights: Long) {
    def sum: Long = zeros + ones + twos + threes + fours + fives + sixes + sevens + eights
    def update: PopulationCounts = PopulationCounts(ones, twos, threes, fours, fives, sixes, sevens + zeros, eights, zeros)
  }

  @tailrec
  private def calculatePopulation(fishes: PopulationCounts, days: Int): Long = {
    if (days == 0) fishes.sum else calculatePopulation(fishes.update, days - 1)
  }

  val parsedInput = parseInput

  val part1 = calculatePopulation(parsedInput, 80)
  val part2 = calculatePopulation(parsedInput, 256)

  println(s"Part 1: ${timeIt {part1}}")
  println(s"Part 2: ${timeIt {part2}}")
}
