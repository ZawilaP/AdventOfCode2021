import scala.annotation.tailrec
import scala.io.Source.fromFile

object day6 extends App {
  private def parseInput(path: String): Map[Int, Long] = {
    val source = fromFile(path)

    val parsedSource = source.getLines().flatMap(_.split(",")).map(_.toInt).toList
    source.close()
    parsedSource.groupMapReduce(identity)(_ => 1L)(_ + _)
  }

  private def updatePopulation(el: Map[Int, Long]): Map[Int, Long] = {
    val numberOfSevens = el.getOrElse(7, 0L)
    val numberOfZeros = el.getOrElse(0, 0L)
    el.flatMap {
      case 0 -> c => Map(6 -> (c + numberOfSevens), 8 -> c)
      case 7 -> c => Map(6 -> (c + numberOfZeros))
      case n -> c => Map(n - 1 -> c)
    }
  }

  @tailrec
  private def calculatePopulation(fishes: Map[Int, Long], days: Int): Long = {
    if (days == 0) fishes.values.sum
    else {
      calculatePopulation(updatePopulation(fishes), days -1)
    }
  }

  val path = "/Users/piotrzawila-niedzwiecki/IdeaProjects/advent_of_code/data/day6_input.txt"
  val parsedInput = parseInput(path)

  val part1 = calculatePopulation(parsedInput, 80)
  val part2 = calculatePopulation(parsedInput, 256)

  println(part1)
  println(part2)
}
