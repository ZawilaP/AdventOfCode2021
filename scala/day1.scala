import scala.annotation.tailrec
import scala.io.Source._

object day1 extends Exercise {
  private def countDepth(measurements: Seq[Int]): Int = {
    @tailrec
    def helper(previousVal: Int, acc: Int, measurements: Seq[Int]): Int = {
      if (measurements.isEmpty) acc
      else if (measurements.head > previousVal) helper(measurements.head, acc + 1, measurements.tail)
      else helper(measurements.head, acc, measurements.tail)
    }

    helper(measurements.head, 0, measurements.tail)
  }

  private def createWindows(measurements: Seq[Int]): Seq[Int] = {
    @tailrec
    def helper(firstVal: Int, secondVal: Int, thirdVal: Int, acc: Seq[Int], measurements: Seq[Int]): Seq[Int] = {
      if (measurements.isEmpty) acc :+ (firstVal+secondVal+thirdVal)
      else helper(secondVal, thirdVal, measurements.head, acc :+ (firstVal+secondVal+thirdVal), measurements.tail)
    }

    helper(measurements.head, measurements.tail.head, measurements.tail.tail.head, Seq.empty, measurements.tail.tail.tail)
  }

  private def parseInput: Seq[Int] = {
    val source = fromFile(path)
    source.getLines.toSeq.map(_.toInt)
  }

  val inputData = parseInput
  val windowed = createWindows(inputData)

  println(s"Part 1: ${countDepth(inputData)}")
  println(s"Part 2: ${countDepth(windowed)}")
}
