import scala.annotation.tailrec
import scala.io.Source._

object day2 extends App {

  case class Distance(horizontal: Int, vertical: Int)

  trait Directions
  case class Forward(length: Int) extends Directions
  case class Up(length: Int) extends Directions
  case class Down(length: Int) extends Directions

  def parseInput(lines: Seq[String]): Seq[Directions] = {
    lines.map{
      case x if x.startsWith("forward") => Forward(x.last.asDigit)
      case x if x.startsWith("down") => Down(x.last.asDigit)
      case x if x.startsWith("up") => Up(x.last.asDigit)
    }
  }

  def calculatePosition(directions: Seq[Directions]): Distance = {
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

  def getFinalResult(lines: Seq[String]): Int = {
    (parseInput _ andThen calculatePosition andThen calculateTotalDistance) (lines)
  }

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

  def getAimedResult(lines: Seq[String]): Int = {
    (parseInput _ andThen calculateAimedPosition andThen calculateTotalDistance) (lines)
  }

  val source = fromFile("/Users/piotrzawila-niedzwiecki/IdeaProjects/advent_of_code/data/day2_input.txt")
  val inputData = source.getLines.toSeq

  val result = getFinalResult(inputData)
  println(s"Showing final: ${result}")

  val aimedResult = getAimedResult(inputData)
  println(s"Showing final: ${aimedResult}")

  source.close()
}
