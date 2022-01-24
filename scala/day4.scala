package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day4 extends Exercise {
  val BOARD_SIZE = 5

  object BoardParser {
    private def parseNumbers(input: String): List[Int] = input.split(",").map(_.toInt).toList

    private def parseBoards(input: List[String]): List[Board] = {
      val boards = input.grouped(BOARD_SIZE).toList
      val parsedNumbers = boards.map(_.flatMap(_.split(" ").filter(_ != "").toList))
      val fullyParsed = parsedNumbers.map(x => x.zipWithIndex).map(x => x.flatMap(y => Map(y._1.toInt -> Field(y._2 / BOARD_SIZE, y._2 % BOARD_SIZE))))
      fullyParsed.map(x => Board(Set.empty[Field], x.toMap))
    }

    def parseInput: (List[Int], List[Board]) = {
      val source = fromFile(path)
      val nonEmptyLines = source.getLines().filter(_.nonEmpty).toList
      val numbers = parseNumbers(nonEmptyLines.head)
      val boards = parseBoards(nonEmptyLines.tail)
      source.close()

      (numbers, boards)
    }
  }

  case class Field(row: Int, col: Int)

  case class Board(visited: Set[Field], notVisited: Map[Int, Field]) {
    def wins: Boolean = {
      val rows = visited.groupBy(_.row).exists(_._2.size == BOARD_SIZE)
      val cols = visited.groupBy(_.col).exists(_._2.size == BOARD_SIZE)
      rows || cols
    }

    def markNumber(number: Int): Board = {
      if (notVisited.contains(number)) Board(visited + notVisited(number), notVisited - number)
      else this
    }

    def calculateSumOfNotVisited: Int = notVisited.keys.sum
  }

  private def part1(numbers: List[Int], boards: List[Board]): Int = {
    @tailrec
    def helper(numbers: List[Int], boards: List[Board], lastCalled: Int): Int = {
      if (boards.exists(_.wins)) boards.filter(_.wins).map(_.calculateSumOfNotVisited * lastCalled).head
      else helper(numbers.tail, boards.map(_.markNumber(numbers.head)), numbers.head)
    }

    helper(numbers, boards, 0)
  }

  private def part2(numbers: List[Int], boards: List[Board]): Int = {
    @tailrec
    def helper(numbers: List[Int], boards: List[Board]): Int = {
      if (boards.length == 1) part1(numbers, boards)
      else helper(numbers.tail, boards.map(_.markNumber(numbers.head)).filter(!_.wins))
    }

    helper(numbers, boards)
  }

  import BoardParser.parseInput

  val (numbers, boards) = parseInput

  println(s"Part 1: ${timeIt {part1(numbers, boards)}}")
  println(s"Part 2: ${timeIt {part2(numbers, boards)}}")
}
