package freskog.day5

import freskog.{BaseSolution, Input}
import zio.UIO
import zio.stream.{ZSink, ZTransducer}

import scala.collection.immutable.SortedSet

object Day5Solution extends BaseSolution:
  override def inputFrom: String = "day5/day5-input.txt"

  def asBinaryStr(input:String):String =
    input
      .replace('B','1')
      .replace('R','1')
      .replace('F','0')
      .replace('L','0')
  
  def parseSeat(input:String):Int =
    Integer.parseInt(asBinaryStr(input),2)
      
  override def solvePart1(input: Input): UIO[String] =
    input.raw
      .transduce(ZTransducer.splitOn("\n"))
      .map(parseSeat)
      .fold(0)(_ max _)
      .map(_.toString)

  override def solvePart2(input: Input): UIO[String] =
    input.raw
      .transduce(ZTransducer.splitOn("\n"))
      .map(parseSeat)
      .run(ZSink.foldLeft[Int,SortedSet[Int]](SortedSet.empty[Int])(_ + _))
      .map(s => SortedSet.from(Range(s.min, s.max)).diff(s).head)
      .map(_.toString)