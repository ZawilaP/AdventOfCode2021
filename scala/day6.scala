import scala.annotation.tailrec
import scala.io.Source.fromFile

object day6 extends Exercise {

  case class PopulationCounts(zeros: Long, ones: Long, twos: Long, threes: Long, fours: Long,
                              fives: Long, sixes: Long, sevens: Long, eights: Long) {
    def sum: Long = zeros + ones + twos + threes + fours + fives + sixes + sevens + eights
    def update: PopulationCounts = PopulationCounts(ones, twos, threes, fours, fives, sixes, sevens + zeros, eights, zeros)
  }

  private def parseInput: PopulationCounts = {
    val source = fromFile(path)

    val parsedSource = source.getLines().flatMap(_.split(",")).map(_.toInt).toList
    source.close()
    val valueCounts = parsedSource.groupMapReduce(identity)(_ => 1L)(_ + _)
    PopulationCounts(valueCounts.getOrElse(0, 0L), valueCounts.getOrElse(1, 0L), valueCounts.getOrElse(2, 0L),
      valueCounts.getOrElse(3, 0L), valueCounts.getOrElse(4, 0L), valueCounts.getOrElse(5, 0L),
      valueCounts.getOrElse(6, 0L), valueCounts.getOrElse(7, 0L), valueCounts.getOrElse(8, 0L))
  }

  @tailrec
  private def calculatePopulation(fishes: PopulationCounts, days: Int): Long = {
    if (days == 0) fishes.sum else calculatePopulation(fishes.update, days - 1)
  }

  val parsedInput = parseInput

  val part1 = calculatePopulation(parsedInput, 80)
  val part2 = calculatePopulation(parsedInput, 256)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
