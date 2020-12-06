package freskog.day6

import freskog.{BaseSolution, Input}
import zio.UIO
import zio.stream.ZTransducer

object Day6Solution extends BaseSolution:
  override def inputFrom: String = "day6/day6-input.txt"

  def parseGroup(group:String):Set[Char] =
    group
      .replaceAll("\n","")
      .foldLeft(Set.empty[Char])(_ + _)
  
  def parseGroup2(group:String):Set[Char] = {
    val (found, rows) = group.toList.foldLeft((Map.empty[Char, Int], 0)) {
      case ((acc, row), '\n') => (acc, row + 1)
      case ((acc, row), c) => (acc.updated(c, acc.getOrElse(c, 0) + 1), row)
    }
    found.collect { case (k, count) if count == rows+1 => k }.toSet
  }
  override def solvePart1(input: Input): UIO[String] =
    input.raw
      .transduce(ZTransducer.splitOn("\n\n"))
      .map(parseGroup andThen (_.size))
      .runSum
      .map(_.toString)
    
  override def solvePart2(input: Input): UIO[String] =
    input.raw
      .transduce(ZTransducer.splitOn("\n\n"))
      .map(parseGroup2 andThen (_.size))
      .runSum
      .map(_.toString)
