import day12.Graph.findPath

import scala.annotation.tailrec
import scala.io.Source.fromFile

object day12 extends Exercise {

  private def parseInput: Graph = { // todo: create a Read[A] class that will allow parsing all the inputs
    val source = fromFile(path)
    val parsedInput = source.getLines.map(_.split("-").map(Cave)).map(x => Passage(x.head, x.tail.head)).toSet
    source.close()
    parsedInput
  }

  type AllowedVisits = Path => Boolean
  case object OneVisit extends AllowedVisits {
    def apply(path: Path): Boolean = path.head.isUpperCase || !path.tail.contains(path.head)
  }
  case object TwoVisits extends AllowedVisits {
    def apply(path: Path): Boolean = path match {
      case nextCave :: path =>
        val smallCaves = path.filter(_.isLowerCase)
        val smallCavesUnique = smallCaves.toSet.size == smallCaves.size
        !nextCave.isStart && (nextCave.isUpperCase || !path.contains(nextCave) || smallCavesUnique)
    }
  }

  case class Cave(value: String) {
    def isUpperCase: Boolean = value.head.isUpper
    def isLowerCase: Boolean = !isUpperCase
    def isStart: Boolean = value == "start"
    def isEnd: Boolean = value == "end"

    def neighbours(graph: Graph): Neighbours = graph.flatMap(_.otherCave(this))
  }

  case class Passage(in: Cave, out: Cave) {
    def otherCave(other: Cave): Option[Cave] = if (in == other) Some(out) else if (out == other) Some(in) else None
  }

  type Graph = Set[Passage]
  object Graph {
    @tailrec
    def findPath(input: Graph, paths: Set[Path], pathFilter: AllowedVisits): Set[Path] = {
      val updatedPath: Set[Path] = paths.filter(!_.head.isEnd)
        .flatMap(path => path.head.neighbours(input).map(path.prepended))
        .filter(pathFilter) ++ paths
      if (updatedPath == paths) paths.filter(_.head.isEnd) else findPath(input, updatedPath, pathFilter)
    }
  }

  type Path = List[Cave]
  type Neighbours = Set[Cave]

  private def part1(input: Graph): Int = findPath(input, Set(List(Cave("start"))), OneVisit).size
  private def part2(input: Graph): Int = findPath(input, Set(List(Cave("start"))), TwoVisits).size

  val input = parseInput

  println(s"Part 1: ${part1(input)}")
  println(s"Part 2: ${part2(input)}")

}
