import scala.io.Source.fromFile
import scala.math.{abs, floor}

object day7 extends App {

  val path = "/Users/piotrzawila-niedzwiecki/IdeaProjects/advent_of_code/data/day7_input.txt"

  private def parseInput(path: String): List[Int] = {
    val source = fromFile(path)
    val parsedSource = source.getLines.next().split(",").map(_.toInt).toList
    source.close()
    parsedSource
  }

  def median(numbers: List[Int]): Int = {
    val sorted = numbers.sorted
    sorted(floor(numbers.length / 2).toInt)
  }

  def gaussSum(num: Int): Int = {
    num * (num+1) / 2
  }

  def part1(numbers: List[Int]): Int = {
    val med = median(numbers)
    numbers.foldLeft(0)((acc, curr) => acc + Math.abs(curr - med))
  }

  def part2(numbers: List[Int]): Int = {
    val avg = floor(numbers.sum.toDouble / numbers.length.toDouble).toInt
    numbers.foldLeft(0)((acc, curr) => acc + gaussSum(abs(curr - avg)))
  }

  val parsedInput = parseInput(path)

  val distance = part1(parsedInput)
  val naturalSumFuelUsage = part2(parsedInput)

  println(distance)
  println(naturalSumFuelUsage)
}
