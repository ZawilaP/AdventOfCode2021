package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.io.Source.fromFile
import scala.math.{abs, floor}

object day7 extends Exercise {

  private def parseInput: List[Int] = {
    val source = fromFile(path)
    val parsedSource = source.getLines.next().split(",").map(_.toInt).toList
    source.close()

    parsedSource
  }

  def median(numbers: List[Int]): Int = {
    val sorted = numbers.sorted
    sorted(floor(numbers.length / 2).toInt)
  }

  def intCumSum(num: Int): Int = num * (num + 1) / 2

  private def part1(numbers: List[Int]): Int = {
    val med = median(numbers)
    numbers.foldLeft(0)((acc, curr) => acc + Math.abs(curr - med))
  }

  private def part2(numbers: List[Int]): Int = {
    val avg = floor(numbers.sum.toDouble / numbers.length.toDouble).toInt
    numbers.foldLeft(0)((acc, curr) => acc + intCumSum(abs(curr - avg)))
  }

  val parsedInput = parseInput

  println(s"Part 1: ${timeIt {part1(parsedInput)}}")
  println(s"Part 2: ${timeIt {part2(parsedInput)}}")
}
