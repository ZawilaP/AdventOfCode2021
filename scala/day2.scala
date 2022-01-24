package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day2 extends Exercise {

  private def parseInput: Seq[Directions] = {
    val source = fromFile(path)
    val input = source.getLines.toSeq.map {
      case x if x.startsWith("forward") => Forward(x.last.asDigit)
      case x if x.startsWith("down") => Down(x.last.asDigit)
      case x if x.startsWith("up") => Up(x.last.asDigit)
    }
    source.close()

    input
  }

  case class Distance(horizontal: Int, vertical: Int) {
    def calculateTotalDistance: Int = horizontal * vertical
  }

  sealed trait Directions
  case class Forward(length: Int) extends Directions
  case class Up(length: Int) extends Directions
  case class Down(length: Int) extends Directions

  private def calculatePosition(directions: Seq[Directions]): Distance = {
    @tailrec
    def helper(horizontal: Int, vertical: Int, acc: Seq[Directions]): Distance = {
      if (acc.isEmpty) Distance(horizontal, vertical)
      else acc.head match {
          case Forward(x) => helper(horizontal + x, vertical, acc.tail)
          case Up(x) => helper(horizontal, vertical - x, acc.tail)
          case Down(x) => helper(horizontal, vertical + x, acc.tail)
        }
    }

    helper(0, 0, directions)
  }

  def calculateAimedPosition(directions: Seq[Directions]): Distance = {
    @tailrec
    def helper(horizontal: Int, vertical: Int, aim: Int, acc: Seq[Directions]): Distance = {
      if (acc.isEmpty) Distance(horizontal, vertical)
      else acc.head match {
          case Forward(x) => helper(horizontal + x, vertical + aim * x, aim, acc.tail)
          case Up(x) => helper(horizontal, vertical, aim - x, acc.tail)
          case Down(x) => helper(horizontal, vertical, aim + x, acc.tail)
        }
    }

    helper(0, 0, 0, directions)
  }

  def part1(lines: Seq[Directions]): Int = calculatePosition(lines).calculateTotalDistance
  def part2(lines: Seq[Directions]): Int = calculateAimedPosition(lines).calculateTotalDistance

  val parsedInput = parseInput

  println(s"Part 1: ${timeIt {part1(parsedInput)}}") //1989265
  println(s"Part 2: ${timeIt {part2(parsedInput)}}") //2089174012
}
