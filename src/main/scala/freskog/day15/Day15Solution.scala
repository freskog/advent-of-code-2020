package freskog.day15

import freskog._
import zio._
import zio.stream.ZTransducer

import scala.collection.immutable.ListMap

object Day15Solution extends BaseSolution:
  override def inputFrom: String = "day15/day15-input.txt"
   
  case class State(currentTurn: Int, lastSpoken:Int, previous:Map[Int, Int]):
    def speak:Int = previous.get(lastSpoken).map(currentTurn - _).getOrElse(0)
    def next =
      State(
        currentTurn + 1,
        speak, 
        previous.updated(lastSpoken, currentTurn)
      )
  
  object State:
    def fromStarting(ns:Int*):State =
      ns.tail.foldLeft(State(1, ns.head, Map.empty)) {
        case (State(turn, spoken, previous), n) => State(turn + 1, n, previous.updated(spoken, turn))
      }
  
  def computeNth(n:Int)(s:State):String = { 
    if n == s.currentTurn then s.lastSpoken.toString
    else computeNth(n)(s.next)
  }

  def solve(input:Input, n:Int):UIO[String] =
    input.raw.transduce(ZTransducer.splitOn(",")).map(_.toInt)
      .runCollect.map(State.fromStarting(_:_*))
      .map(computeNth(n))
  
  override def solvePart1(input: Input): UIO[String] =
    solve(input, 2020)

  override def solvePart2(input: Input): UIO[String] =
    solve(input, 30000000)