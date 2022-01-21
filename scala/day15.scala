package solutions

import commons.Exercise

import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.language.implicitConversions

object day15 extends Exercise {
  private def parseInput: Fields = {
    val source = fromFile(path)
    val input = source.getLines().toList
    val fields = {
      for {
        (line, row) <- input.zipWithIndex
        (height, col) <- line.zipWithIndex
      } yield Field(row, col) -> height.asDigit
    }.toMap

    fields
  }

  case class Field(row: Int, col: Int) {
    def neighbours: Neighbours = Neighbours(Field(row, col - 1), Field(row, col + 1), Field(row - 1, col), Field(row + 1, col))
  }

  case class Neighbours(left: Field, right: Field, above: Field, below: Field) {
    def toSet = Set(left, right, above, below)
  }

  type Fields = Map[Field, Int]

  implicit def mapToFields(m: Fields): BetterFields = new BetterFields(m)

  class BetterFields(elements: Fields) {
    def expand(xTimes: Int, yTimes: Int): Map[Field, Int] = {
      val (width, height) = size
      List.tabulate(xTimes, yTimes)((x, y) =>
        elements.toSeq.map(el =>
          Field(x * width + el._1.row, y * height + el._1.col) -> (1 + (el._2 -1 + x + y) % 9)
        )).flatten.flatten.toMap
    }

    private def size: (Int, Int) = {
      val lastField = elements.keys.maxBy(el => el.row * el.col)
      (lastField.row + 1, lastField.col + 1)
    }
  }

  def findCheapestPath(fields: Fields): Int = {
    val (start, end) = (Field(0, 0), fields.keys.maxBy(p => p.row * p.col))

    @tailrec
    def dijkstra(toVisit: Set[Field], pathRisk: Fields): Int = {
      val point = toVisit.minBy(pathRisk)
      if (point == end) pathRisk(end)
      else {
        val (nextTodo, nextRisk) = point.neighbours.toSet.filter(fields.contains) // keep only legal points within grid
          .filter(next => !pathRisk.contains(next)) // filter out visited as going back will never be optimal
          .foldLeft((toVisit - point, pathRisk)) { case ((toVisit, risk), next) => // for remaining unvisited add checking route cost
            (toVisit + next, risk.updated(next, risk(point) + fields(next))) // and add it's risk to the current path
          }
        dijkstra(nextTodo, nextRisk)
      }
    }

    dijkstra(Set(start), Map(start -> 0))
  }

  private def part1(parsedInput: Map[Field, Int]): Int = findCheapestPath(parsedInput)
  private def part2(parsedInput: Map[Field, Int]): Int = findCheapestPath(parsedInput.expand(5, 5))

  val input = parseInput

  println(s"Part 1: ${part1(input)}") // 447
  println(s"Part 2: ${part2(input)}") // 2825
}
