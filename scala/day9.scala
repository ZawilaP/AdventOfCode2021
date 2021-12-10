import scala.annotation.tailrec
import scala.io.Source.fromFile

object day9 extends App {

  type Fields = Map[Field, Int]

  case class Neighbours(left: Field, right: Field, above: Field, below: Field) {
    def toSet = Set(left, right, above, below)
  }

  case class Field(row: Int, col: Int) {
    private def neighbours: Neighbours = {
      Neighbours(Field(row, col - 1), Field(row, col + 1), Field(row + 1, col), Field(row - 1, col))
    }

    def belowNeighbours(fields: Fields): Boolean = {
      neighbours.toSet.forall(this.belowNeighbour(_, fields))
    }

    private def belowNeighbour(neighbour: Field, fields: Fields): Boolean = {
      if (fields.contains(neighbour)) {
        fields(this) < fields(neighbour)
      }
      else true
    }

    def findBasin(fields: Fields): Int = {
      @tailrec
      def helper(currentBasin: Set[Field]): Set[Field] = {
        val updatedBasin = currentBasin.flatMap(_.neighbours.toSet)
          .filter(neighbour => fields.get(neighbour).exists(_ < 9) &&
            !currentBasin.contains(neighbour)) ++ currentBasin

        if (updatedBasin == currentBasin) currentBasin else helper(updatedBasin)
      }

      helper(Set(this)).size
    }
  }

  private def parseInput(path: String): Fields = {
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

  private def part1(parsedInput: Fields): Int = {
    parsedInput.toSeq.map(a => if (a._1.belowNeighbours(input)) a._2 + 1 else 0).sum
  }

  private def part2(parsedInput: Fields): Int = {
    parsedInput.toSeq.filter(_._1.belowNeighbours(input)).map(_._1.findBasin(parsedInput)).sorted.takeRight(3).product
  }

  val path = "/Users/piotrzawila-niedzwiecki/IdeaProjects/advent_of_code/data/day9_input.txt"

  val input = parseInput(path)

  println(s"Part 1: ${part1(input)}")
  println(s"Part 2: ${part2(input)}")
}
