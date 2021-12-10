import scala.io.Source.fromFile

object day8 extends Exercise {

  case class UnparsedDigit(value: String)

  case class TrainAndTest(train: Seq[UnparsedDigit], test: Seq[UnparsedDigit])

  private def parseInput: Seq[TrainAndTest] = {
    val source = fromFile(path)
    val parsed = source.getLines.toList.map(_.split(" \\| ")).map{
      case Array(l, r) => TrainAndTest(l.split(" ").map(UnparsedDigit), r.split(" ").map(UnparsedDigit))
    }
    source.close()
    parsed
  }

  private def part1(input: Seq[TrainAndTest]): Int = {
    val test = input.map(_.test)
    test.flatten.map(_.value).count(x => Seq(2, 3, 4, 7).contains(x.length))
  }

  val parsedInput = parseInput

  println(s"Part 1: ${part1(parsedInput)}")
//  println(s"Part 2: ${part2(input)}")
}
