package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day11 extends Exercise {

  private def parseInput: Fields = { // todo: create a Read[A] class that will allow parsing all the inputs, as this is duplicated
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

  type Fields = Map[Field, Int]

  object Fields {
    @tailrec
    private def findFlashes(fields: Fields, flashes: Set[Field]): Set[Field] = {
      val updatedFlashes = flashes.flatMap(_.neighbours.toSet)
        .filter(candidate => fields.get(candidate)
          .exists(_ + candidate.neighbours.toSet.intersect(flashes).size > 9)) ++ flashes
      if (updatedFlashes == flashes) flashes else findFlashes(fields, updatedFlashes)
    }

    private def getNextFields(fields: Fields): Fields = {
      val incremented = fields.view.mapValues(_ + 1).toMap
      val flashes = findFlashes(incremented, incremented.filter(_._2 > 9).keySet)
      incremented.map { case (point, value) =>
        (point,
          if (flashes.contains(point)) 0
          else value + point.neighbours.toSet.count(flashes.contains))
      }
    }

    @tailrec
    def computeFields(fields: Fields, count: Int, acc: List[Fields]): List[Fields] = {
      if (count == 0) acc
      else computeFields(fields, count - 1, acc.prepended(getNextFields(acc.head)))
    }

    @tailrec
    def indexWhenAllFlash(fields: Fields, steps: Int): Int = {
      if (fields.values.forall(_ == 0)) steps
      else {
        indexWhenAllFlash(getNextFields(fields), steps + 1)
      }
    }
  }

  case class Neighbours(left: Field, right: Field, above: Field, below: Field, leftUp: Field, leftDown: Field, rightUp: Field, rightDown: Field) {
    def toSet = Set(left, right, above, below, leftUp, leftDown, rightUp, rightDown)
  }

  case class Field(row: Int, col: Int) {
    def neighbours: Neighbours = {
      Neighbours(Field(row, col - 1), Field(row, col + 1), Field(row + 1, col), Field(row - 1, col),
        Field(row + 1, col + 1), Field(row + 1, col - 1), Field(row - 1, col + 1), Field(row - 1, col - 1))
    }
  }

  import Fields._

  private def part1(input: Fields): Int = {
    computeFields(input, 100, List(input)).map(_.values.count(_ == 0)).sum
  }

  private def part2(input: Fields): Int = {
    indexWhenAllFlash(input, 0)
  }

  val input = parseInput

  println(s"Part 1: ${timeIt {part1(input)}}")
  println(s"Part 2: ${timeIt {part2(input)}}")
}
