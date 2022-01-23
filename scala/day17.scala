package solutions

import commons.Exercise
import commons.Time.timeIt

import scala.io.Source.fromFile

object day17 extends Exercise {

  private def parseInput: TargetArea = {
    val source = fromFile(path)
    val input = source.getLines().toList.head
    val coordinates = input.split(": ").last.split(", ")
      .flatMap(_.drop(2).split("\\.\\.").map(_.toInt))

    TargetArea(coordinates(0), coordinates(1), coordinates(2), coordinates(3))
  }

  case class Point(x: Int, y: Int) {
    private def +(other: Point): Point = Point(x + other.x, y + other.y)
    private def step: Point = Point(Math.max(x - 1, 0), y - 1)

    def iterate: Iterator[Point] = {
      Iterator.iterate((Point(0, 0), this))(acc => (acc._1 + acc._2, acc._2.step)).map(_._1)
    }
  }

  case class TargetArea(minX: Int, maxX: Int, minY: Int, maxY: Int) {
    def highestY: Int = minY * (minY + 1) / 2
    def hitCount: Int = trajectories.count(intersection)

    private def trajectories: Seq[Iterator[Point]] = for {
      vx <- Range(1, maxX + 1)
      vy <- Range(minY, -minY + 1)
      points = Point(vx, vy).iterate.takeWhile(point => point.x <= maxX && point.y >= minY)
    } yield points

    private def intersection(trajectory: Iterator[Point]): Boolean = {
      trajectory.exists(el => el.x >= minX && el.y <= maxY)
    }
  }

  private def part1(input: TargetArea): Int = input.highestY
  private def part2(input: TargetArea): Int = input.hitCount

  val input = parseInput

  println(s"Part 1: ${timeIt {part1(input)}}") // 15400
  println(s"Part 2: ${timeIt {part2(input)}}") // 5280
}
