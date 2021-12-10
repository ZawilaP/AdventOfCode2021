import scala.annotation.tailrec
import scala.io.Source._

object day2 extends Exercise {

  case class Distance(horizontal: Int, vertical: Int)

  trait Directions
  case class Forward(length: Int) extends Directions
  case class Up(length: Int) extends Directions
  case class Down(length: Int) extends Directions

  private def parseInput: Seq[Directions] = {
    val source = fromFile(path)
    val input = source.getLines.toSeq.map{
      case x if x.startsWith("forward") => Forward(x.last.asDigit)
      case x if x.startsWith("down") => Down(x.last.asDigit)
      case x if x.startsWith("up") => Up(x.last.asDigit)
    }
    source.close()
    input
  }

  private def calculatePosition(directions: Seq[Directions]): Distance = {
    @tailrec
    def helper(horizontal: Int, vertical: Int, acc: Seq[Directions]): Distance = {
      if (acc.isEmpty) Distance(horizontal, vertical)
      else {
        acc.head match {
          case Forward(x) => helper(horizontal + x, vertical, acc.tail)
          case Up(x) => helper(horizontal, vertical - x, acc.tail)
          case Down(x) => helper(horizontal, vertical + x, acc.tail)
        }
      }
    }

    helper(0,0, directions)
  }

  def calculateTotalDistance(distance: Distance): Int = distance.horizontal * distance.vertical



  def calculateAimedPosition(directions: Seq[Directions]): Distance = {
    @tailrec
    def helper(horizontal: Int, vertical: Int, aim: Int, acc: Seq[Directions]): Distance = {
      if (acc.isEmpty) Distance(horizontal, vertical)
      else {
        acc.head match {
          case Forward(x) => helper(horizontal + x, vertical + aim * x, aim, acc.tail)
          case Up(x) => helper(horizontal, vertical, aim - x, acc.tail)
          case Down(x) => helper(horizontal, vertical, aim + x, acc.tail)
        }
      }
    }

    helper(0, 0, 0, directions)
  }

  def part1(lines: Seq[Directions]): Int = {
    (calculatePosition _ andThen calculateTotalDistance) (lines)
  }

  def part2(lines: Seq[Directions]): Int = {
    (calculateAimedPosition _ andThen calculateTotalDistance) (lines)
  }

  val parsedInput = parseInput

  println(s"Part 1: ${part1(parsedInput)}")
  println(s"Part 2: ${part2(parsedInput)}")
}
