import scala.io.Source.fromFile

object day5 extends Exercise {

  private def parseInput: List[Line] = {
    val source = fromFile(path)
    val parsedSource = source.getLines().toList.map(_.split(",| -> ")).map {
      case Array(x1: String, y1: String, x2: String, y2: String) => Line(Point(x1.toInt, y1.toInt), Point(x2.toInt, y2.toInt))
    }
    source.close()
    parsedSource
  }

  case class Point(x: Int, y: Int) {
    def equal(a: Point): Boolean = this.x == a.x && this.y == a.y
  }

  case class Line(a: Point, b: Point) {
    def isStraight: Boolean = a.x == b.x || a.y == b.y

    def points: Seq[Point] = {
      val xs = Range(a.x, b.x, if (a.x > b.x) -1 else 1)
      val ys = Range(a.y, b.y, if (a.y > b.y) -1 else 1)
      if (isStraight) {
        for {
          x <- xs
          y <- ys
        } yield Point(x, y)
      } else {
        xs.zip(ys).map(x => Point(x._1, x._2))
      }
    }
  }


  private def calculateCount(input: List[Line], straight: Boolean): Int = {
    (if (straight) {
      input.filter(_.isStraight)
    } else {
      input
    }).flatMap(_.points).groupBy(identity).count(_._2.size > 1)
  }

  val parsedInput = parseInput

  val part1 = calculateCount(parsedInput, straight = true)
  val part2 = calculateCount(parsedInput, straight = false)

  println(s"Part 1: $part1")
  println(s"Part 2: $part2")
}
