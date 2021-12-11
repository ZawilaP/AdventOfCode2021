import scala.annotation.tailrec
import scala.io.Source.fromFile
import scala.reflect.ClassTag

object day10 extends Exercise {

  trait InputType
  case class Valid(value: Seq[Char]) extends InputType
  case class Invalid(value: Char) extends InputType

  case class Input(valid: Seq[Valid], invalid: Seq[Invalid])

  private val bracketsMapping = Map(
    ')' -> '(',
    ']' -> '[',
    '}' -> '{',
    '>' -> '<')

  private def parseInput: Input = {
    @tailrec
    def helper(value: String, acc: List[Char]): InputType = {
      (value, acc) match {
        case (x, y) if x.isEmpty => Valid(y)
        case (_, y) if y.isEmpty => helper(value.tail, value.head :: acc)
        case (x, y) if bracketsMapping.contains(x.head) && y.head != bracketsMapping(x.head) => Invalid(x.head)
        case (x, y) if bracketsMapping.contains(x.head) && y.head == bracketsMapping(x.head) => helper(value.tail, y.tail)
        case (_, _) => helper(value.tail, value.head :: acc)
      }
    }

    def filter[T : ClassTag](data: Seq[_]): Seq[T] = data collect { case t: T => t }

    val source = fromFile(path)
    val input = source.getLines().toSeq.map(helper(_, List.empty[Char]))
    source.close()
    Input(filter[Valid](input), filter[Invalid](input))
  }

  private val part1Scoring = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137)
  private val part2Scoring = Map(
    '(' -> 1L,
    '[' -> 2L,
    '{' -> 3L,
    '<' -> 4L)

  private def part1(input: Seq[Invalid]): Int = {
    input.map(x => part1Scoring(x.value)).sum
  }

  private def part2(input: Seq[Valid]): Long = {
    val scores = input.map(_.value.foldLeft(0L)((acc, char) => acc * 5L + part2Scoring(char))).sorted
    scores(scores.length / 2)
  }

  val input = parseInput

  println(s"Part 1: ${part1(input.invalid)}")
  println(s"Part 2: ${part2(input.valid)}")
}
