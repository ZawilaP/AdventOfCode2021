package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.io.Source.fromFile

object day8 extends Exercise {

  private def parseInput: Seq[TrainAndTest] = {
    val source = fromFile(path)
    val parsed = source.getLines.toList.map(_.split(" \\| ")).map {
      case Array(l, r) => TrainAndTest(l.split(" ").toSeq, r.split(" ").toSeq)
    }
    source.close()

    parsed
  }

  implicit class IntOps(value: String) {
    def containsAllElements(other: String): Boolean = other.map(value.contains(_)).reduce(_ && _)
  }

  case class TrainAndTest(train: Seq[String], test: Seq[String])

  private def part1(input: Seq[TrainAndTest]): Int = {
    val test = input.map(_.test)
    test.flatten.count(x => Seq(2, 3, 4, 7).contains(x.length))
  }

  private def part2(input: Seq[TrainAndTest]): Int = {
    /**
     * String of length 5 can be either 2, 3 or 5. In this group only 5 has both upper left element and center, which
     * number can be derived from 4 and 1. Furthermore 3 contains 1, but 2 doesn't.
     *
     * String of length 6 can be either 0, 6, 9. Similarly, in this set, only 0 doesn't have center element and
     * 6 doesn't contain 1, whilst 9 does.
     */
    def infer(input: TrainAndTest): Int = {
      input.train.sortBy(_.length) match {
        case Seq(one, seven, four, fiveEl1, fiveEl2, fiveEl3, sixEl1, sixEl2, sixEl3, eight) =>
          val centerUpperLeft = four.diff(one)
          val (five, twoThree) = List(fiveEl1, fiveEl2, fiveEl3).partition(_.containsAllElements(centerUpperLeft))
          val (sixNine, zero) = List(sixEl1, sixEl2, sixEl3).partition(_.containsAllElements(centerUpperLeft))
          val (three, two) = twoThree.partition(_.containsAllElements(one))
          val (nine, six) = sixNine.partition(_.containsAllElements(one))

          val mapping = List(zero.head, one, two.head, three.head, four, five.head, six.head, seven, eight, nine.head).
            map(_.sorted).zipWithIndex.toMap
          input.test.map(x => mapping(x.sorted)).mkString.toInt
      }
    }

    input.map(infer).sum
  }

  val parsedInput = parseInput

  println(s"Part 1: ${timeIt {part1(parsedInput) }}")
  println(s"Part 2: ${timeIt {part2(parsedInput) }}")
}
