import scala.annotation.tailrec
import scala.io.Source.fromFile

object day13 extends Exercise {

  private def parseInput: Input = {
    val source = fromFile(path)
    val (instructions, dots) = source.getLines().filter(_.nonEmpty).partition(_.startsWith("fold"))
    val parsedDots = dots.map(x => {
      val split = x.split(",")
      Dot(split.head.toInt, split.tail.head.toInt)
    }).toSet
    val parsedInstructions = instructions.toSeq.map(x => {
      val split = x.split("=")
      split.head.last.toString match {
        case "y" => Vertical(split.tail.head.toInt)
        case "x" => Horizontal(split.tail.head.toInt)
      }
    })
    source.close()
    Input(parsedDots, parsedInstructions)
  }

  case class Dot(x: Int, y: Int)

  sealed trait Instruction {
    def fold(dots: Dots): Dots
  }
  case class Vertical(value: Int) extends Instruction {
    def fold(dots: Dots): Dots = {
      val filteredSet = dots.filter(dot => dot.y > value)
      val mappedSet = filteredSet.map(dot => Dot(dot.x, 2 * value - dot.y))
      dots -- filteredSet ++ mappedSet
    }
  }
  case class Horizontal(value: Int) extends Instruction {
    def fold(dots: Dots): Dots = {
      val filteredSet = dots.filter(dot => dot.x > value)
      val mappedSet = filteredSet.map(dot => Dot(2 * value - dot.x, dot.y))
      dots -- filteredSet ++ mappedSet
    }
  }

  type Dots = Set[Dot]
  type Instructions = Seq[Instruction]

  implicit class DotsVisualize(dots: Dots) {
    def display: String =
      (dots.map(_.x).min to dots.map(_.x).max).map{row =>
        (dots.map(_.y).min to dots.map(_.y).max).map{col =>
          if (dots.contains(Dot(row, col))) 'X' else ' '
        }.mkString
      }.mkString("\n")
  }

  case class Input(dots: Dots, instructions: Instructions) {
    @tailrec
    final def executeInstructions: Dots = {
      if (instructions.isEmpty) dots
      else Input(instructions.head.fold(dots), instructions.tail).executeInstructions
    }
  }

  private def part1(input: Input): Int = input.instructions.head.fold(input.dots).size
  private def part2(input: Input): String = "\n" + input.executeInstructions.display

  val input = parseInput

  println(s"Part 1: ${part1(input)}")
  println(s"Part 2: ${part2(input)}")

}
