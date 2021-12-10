import scala.annotation.tailrec
import scala.io.Source.fromFile

object day6 extends Exercise {

  private def parseInput: Map[Int, Long] = {
    val source = fromFile(path)

    val parsedSource = source.getLines().flatMap(_.split(",")).map(_.toInt).toList
    source.close()
    parsedSource.groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  private def updatePopulation(el: Map[Int, Long]): Map[Int, Long] = {
    val numberOfSevens = el.getOrElse(7, 0L)
    val numberOfZeros = el.getOrElse(0, 0L)
    el.flatMap {
      case 0 -> value => Map(6 -> (value + numberOfSevens), 8 -> value)
      case 7 -> value => Map(6 -> (value + numberOfZeros))
      case n -> value => Map(n - 1 -> value)
    }
  }

  @tailrec
  private def calculatePopulation(fishes: Map[Int, Long], days: Int): Long = {
    if (days == 0) fishes.values.sum
    else {
      calculatePopulation(updatePopulation(fishes), days -1)
    }
  }

  val parsedInput = parseInput

  val part1 = calculatePopulation(parsedInput, 80)
  val part2 = calculatePopulation(parsedInput, 256)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
