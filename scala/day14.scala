package solutions

import cats.implicits._
import commons.Exercise
import commons.Time.timeIt

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day14 extends Exercise {

  type Instructions = Map[String, String]
  type PairCount = Map[String, Long]
  type LetterCount = Map[String, Long]

  case class State(pairCounts: PairCount, letterCounts: LetterCount) {
    def ++(other: State): State = State(this.pairCounts combine other.pairCounts, this.letterCounts combine other.letterCounts)
  }

  case class Input(letters: String, instructions: Instructions) {
    def solve(steps: Int): Long = {
      @tailrec
      def helper(pairs: PairCount, counts: LetterCount, steps: Int): Long = {
        if (steps == 0) counts.values.max - counts.values.min
        else {
          val updatedState = pairs.map(updateValues).reduce(_ ++ _)
          helper(updatedState.pairCounts.filter(_._2 > 0), counts combine updatedState.letterCounts, steps - 1)
        }
      }

      val pairCounts = createPairs(letters)
      val letterCounts = Map(letters.map(char => char.toString -> letters.count(_ == char).toLong): _*)
      helper(pairCounts, letterCounts, steps)
    }

    def createPairs(letters: String): PairCount = {
      @tailrec
      def helper(letters: String, acc: PairCount): PairCount = {
        if (letters.length < 2) acc
        else helper(letters.tail, acc combine Map(letters.take(2) -> 1L))
      }

      helper(letters, Map.empty)
    }

    def updateValues(pair: (String, Long)): State = {
      pair match {
        case (pair, value) =>
          val firstEl = pair.head
          val secondEl = pair.last
          val addedLetter = instructions(pair)
          val firstNewPair = firstEl + addedLetter
          val secondNewPair = addedLetter + secondEl
          State(Map(firstNewPair -> value, secondNewPair -> value), Map(addedLetter -> value))
      }
    }
  }

  private def parseInput: Input = {
    val source = fromFile(path)
    val (instructions, _) = source.getLines().filter(_.nonEmpty).toSeq.partition(_.contains(" -> "))
    source.close()
    Input("KHSSCSKKCPFKPPBBOKVF", instructions.flatMap(x => {
      val split = x.split(" -> ")
      Map(split.head -> split.tail.head)
    }).toMap)
  }

  val input = parseInput

  private def part1(input: Input): Long = input.solve(10)
  private def part2(input: Input): Long = input.solve(40)

  println(s"Part 1: ${timeIt {part1(input)}}")
  println(s"Part 2: ${timeIt {part2(input)}}")
}