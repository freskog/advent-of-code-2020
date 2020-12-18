package freskog.day13

import zio._
import freskog._
import zio.stream.{ZSink, ZStream, ZTransducer}

import scala.util.Try

object Day13Solution extends BaseSolution:
  override def inputFrom: String = "day13/day13-input.txt"

  def waitingTime(ts:Int, busId:Int):Int = 
    (busId * ((ts / busId)+1)) - ts
  
  override def solvePart1(input: Input): UIO[String] =
    input.raw.transduce(ZTransducer.splitLines).peel(ZSink.head).use {
      case (None, _) => ZIO.dieMessage("No target ts found!")
      case (Some(ts), tail) =>
        tail
          .transduce(ZTransducer.splitOn(","))
          .filterNot(_ == "x")
          .map(_.toInt)
          .map(id => id -> waitingTime(ts.toInt, id))
          .run(ZSink.collectAllToSet)
          .map(_.minBy(_._2))
          .map { case (id, time) => (id * time).toString }
    }

  /**
   * Part II
   * 
   * Chinese remainder theorem [https://en.wikipedia.org/wiki/Chinese_remainder_theorem] states
   * that if we have a series of linear (?) congruence equations like
   *
   * ts =_ 0 mod (busId0)
   * ts =_ 1 mod (busId1)
   * ts =_ 2 mod (busId2)
   * ...
   * 
   * Following is a stolen implementation of the chinese remainder theorem
   * based on : https://rosettacode.org/wiki/Chinese_remainder_theorem#Scala
   */
    
  def mod(a:Long, n:Long):Long =
    if a % n < 0 then n + (a % n) else a % n
    
  def chineseRemainder(a: List[Long], n: List[Long]): Long =
    val prod = n.product
    
    def iter(n: List[Long], a: List[Long], sm: Long): Long = 
      def mulInv(a: Long, b: Long): Long = 
        def loop(a: Long, b: Long, x0: Long, x1: Long): Long = 
          if (a > 1) loop(b, mod(a, b), x1 - (a / b) * x0, x0) else x1
        if b == 1 then 1 else 
          val x1 = loop(a, b, 0, 1)
          if (x1 < 0) x1 + b else x1
      if n.nonEmpty then 
        val p = prod / n.head
        iter(n.tail, a.tail, sm + a.head * mulInv(p, n.head) * p)
      else sm
         
    mod(iter(n, a, 0), prod)

  override def solvePart2(input:Input):UIO[String] =
    input.raw
      .transduce(ZTransducer.splitLines)
      .drop(1)
      .transduce(ZTransducer.splitOn(","))
      .zipWithIndex
      .filterNot(_._1 == "x")
      .map { case (busId, idx) => (-1 * idx, busId.toLong) }
      .runCollect
      .map { pairs =>
        val (idxes, ids) = pairs.unzip
        val solution = chineseRemainder(idxes.toList, ids.toList)
        solution.toString
      }
     