package freskog.day9

import freskog._
import zio._
import zio.stream._

object Day9Solution extends BaseSolution:
  override def inputFrom: String = "day9/day9-input.txt"

  def valid(i:Long, last25:Set[Long]):Boolean =
    last25.foldLeft((last25,Set.empty[Long])) {
      case ((remaining, sums), n) => (remaining - n, sums ++ ((remaining - n).map(_ + n)))
    }._2.contains(i)
  
  def longsFrom(input:Input): ZStream[Any, Nothing, Long] =
    input.raw.transduce(ZTransducer.splitOn("\n")).map(_.toLong)

  def findTarget(input: Input): ZIO[Any, Nothing, Long] =
    longsFrom(input)
      .fold((List.empty[Long], List.empty[Long])) {
        case ((preamble,     _), i) if preamble.length <= 25 => (i :: preamble, Nil)
        case ((  last25, found), i) if !valid(i, last25.toSet) => (i :: last25.init, i :: found)
        case ((  last25, found), i) => (i :: last25.init, found)
      }.map(_._2.head)
      
  def test(remaining:List[Long], target:Long, len: Int):Long =
    remaining.sliding(len).collectFirst {
      case l if l.sum == target => l.min + l.max
    }.getOrElse(test(remaining, target, len + 1))
      
  
  override def solvePart1(input:Input):UIO[String] =
    findTarget(input).map(_.toString)

  override def solvePart2(input:Input):UIO[String] =
    findTarget(input).map(
      test(input.asLines().map(_.toLong).toList, _, 2).toString
    )
  