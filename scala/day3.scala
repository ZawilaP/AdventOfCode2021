import java.lang.Integer.parseInt
import scala.annotation.tailrec
import scala.io.Source.fromFile

object day3 extends Exercise {

  private def parseInput: Vector[Vector[Bit]] = {
    val source = fromFile(path)
    val input = source.getLines().toVector.transpose.map(_.map{case '1' => One
    case '0' => Zero})
    source.close()
    input
  }

  sealed trait Bit
  case object One extends Bit {
    override def toString: String = "1"
  }
  case object Zero extends Bit {
    override def toString: String = "0"
  }

  sealed trait Rating
  case object Oxygen extends Rating
  case object CO2 extends Rating

  def part1(transposed: Vector[Vector[Bit]]): Int = {
    val length = transposed.head.length / 2
    val gamma = transposed.map(_.count(_ == One)).map{
      case num if num > length => One
      case _ => Zero
    }
    val epsilon = gamma.map {
      case One => Zero
      case Zero => One
    }

    parseInt(gamma.mkString, 2) *
      parseInt(epsilon.mkString, 2)
  }

  private def part2(transposed: Vector[Vector[Bit]]): Int = {
    @tailrec
    def helper(transposedInput: Vector[Vector[Bit]], rating: Rating, pointer: Int = 0): Int = {
      if (transposedInput.forall(_.length == 1)) {
        Integer.parseInt(transposedInput.flatten.mkString, 2)
      } else {
        val column = transposedInput(pointer)
        val length = column.length / 2
        val oneCount = column.count(_ == One)
        val zeroCount = column.count(_ == Zero)
        val charToFilter = rating match {
          case Oxygen if oneCount == zeroCount || oneCount > length => Zero
          case Oxygen => One
          case CO2 if oneCount == zeroCount || oneCount > length  => One
          case CO2 => Zero
        }
        val filtered = transposedInput.transpose.filterNot(row => row(pointer) == charToFilter).transpose
        helper(filtered, rating, pointer + 1)
      }
    }

    helper(transposed, Oxygen) * helper(transposed, CO2)
  }

  val input = parseInput

  println(s"Part 1: ${part1(input)}")
  println(s"Part 2: ${part2(input)}")
}
