package solutions

import cats.implicits._
import commons.Exercise
import commons.Time.timeIt

import scala.io.Source.fromFile

object day16 extends Exercise {

  private val mappings: Map[Char, String] = Map(
    '0' -> "0000",
    '1' -> "0001",
    '2' -> "0010",
    '3' -> "0011",
    '4' -> "0100",
    '5' -> "0101",
    '6' -> "0110",
    '7' -> "0111",
    '8' -> "1000",
    '9' -> "1001",
    'A' -> "1010",
    'B' -> "1011",
    'C' -> "1100",
    'D' -> "1101",
    'E' -> "1110",
    'F' -> "1111")

  private def hexadecimalToBinary(hex: String): String = hex.map(mappings).mkString
  private def binaryToDecimal(bin: String): Long = BigInt(bin, 2).toLong

  case class PacketInfo(size: Int, packets: List[Packet])

  trait Packet {
    def version: Long
    def sum: Long
    def size: Int
    def evaluate: Long
  }

  case class Literal(version: Long, value: Long, size: Int) extends Packet {
    def sum: Long = version
    def evaluate: Long = value
  }

  object Literal {
    def apply(data: String, version: Long): Option[Literal] = {
      val (ones, zeros) = data.grouped(5).toList.span(_.head == '1')
      for {
        value <- Some(ones.map(_.drop(1)).mkString + zeros.head.drop(1).mkString)
        size  <- Some(6 + (ones.size + 1) * 5)
      } yield Literal(version, binaryToDecimal(value), size)
    }
  }

  case class Operator(version: Long, typeId: Long, packets: List[Packet], size: Int) extends Packet {
    def sum: Long = version + packets.map(_.sum).sum
    def evaluate: Long = (typeId, packets) match {
      case (0, xs) => xs.map(_.evaluate).sum
      case (1, xs) => xs.map(_.evaluate).product
      case (2, xs) => xs.map(_.evaluate).min
      case (3, xs) => xs.map(_.evaluate).max
      case (5, List(x, y)) => if (x.evaluate > y.evaluate) 1 else 0
      case (6, List(x, y)) => if (x.evaluate < y.evaluate) 1 else 0
      case (7, List(x, y)) => if (x.evaluate == y.evaluate) 1 else 0
    }
  }

  object Operator {
    def apply(data: String, version: Long, typeId: Long, lengthId: Int): Option[Operator] = {
      val packetInfo = lengthId match {
        case 1 => Iterator
            .iterate(PacketInfo(18, List.empty[Packet]))(info => parse(data.drop(info.size), info.size, info.packets).get)
            .drop(binaryToDecimal(data.slice(7, 18)).toInt)
            .next()
        case _ => Iterator
            .iterate(PacketInfo(22, List.empty[Packet]))(info => parse(data.drop(info.size), info.size, info.packets).get)
            .dropWhile(_.size < binaryToDecimal(data.slice(7, 22)) + 22)
            .next()
      }
      Some(Operator(version, typeId, packetInfo.packets, packetInfo.size))
    }
  }

  def parseInput: Option[Packet] = {
    for {
      source <- Some(fromFile(path))
      input  <- Some(source.getLines().toList.head)
    } yield parse(hexadecimalToBinary(input)).get.packets.head
  }

  def parse(data: String, size: Int = 0, packets: List[Packet] = List.empty[Packet]): Option[PacketInfo] = {
    for {
      version   <- Some(binaryToDecimal(data.take(3)))
      typeId    <- Some(binaryToDecimal(data.slice(3, 6)))
      newPacket = if (typeId == 4) Literal(data.drop(6), version)
                else Operator(data, version, typeId, data(6).asDigit)
    } yield PacketInfo(size + newPacket.get.size, packets :+ newPacket.get)
  }

  def part1(input: Option[Packet]): Long = input.get.sum
  def part2(input: Option[Packet]): Long = input.get.evaluate

  val input = parseInput

  println(s"Part 1: ${timeIt {part1(input)}}") // 886
  println(s"Part 2: ${timeIt {part2(input)}}") // 184487454837
}
