package freskog.day1

import freskog._
import zio._

object Day1Solution extends BaseSolution:

  override def inputFrom: String = "day1/day1-input.txt"

  sealed trait Tree:
    def add(n:Int):Tree
    def findSumPath(sum:Int):Chunk[Int]
  
  object Tree:
    def fromInts(maxDepth:Int, ints:Chunk[Int]):Tree =
      ints.foldLeft[Tree](Root(maxDepth, Chunk.empty))(_ add _)
  
  case class Root(maxDepth:Int, children:Chunk[Tree]) extends Tree:
    def add(n:Int):Tree = Root(maxDepth, Node(maxDepth, n, Chunk.empty) +: children.map(_.add(n)))
    def findSumPath(sum: Int): Chunk[Int] = 
      children.foldWhile[Chunk[Int]](Chunk.empty)(_.isEmpty)((_,n) => n.findSumPath(sum))
  
  case class Node(remainingDepth:Int, value:Int, children:Chunk[Tree]) extends Tree:
    def add(n:Int):Tree = if remainingDepth == 0 then this else
      Node(remainingDepth, value, Node(remainingDepth - 1, n, Chunk.empty) +: children.map(_.add(n)))

    def findSumPath(sum:Int):Chunk[Int] = 
      if sum - value == 0 && remainingDepth == 0 then Chunk(value)
      else if sum - value < 0 then Chunk.empty
      else 
        val path = children.foldWhile[Chunk[Int]](Chunk.empty)(_.isEmpty)((_,n) => n.findSumPath(sum - value))
        if path.isEmpty then Chunk.empty else value +: path
  
  def solve(terms:Int, input:Input):UIO[String] =
    UIO(Tree.fromInts( terms - 1, input.asInts()).findSumPath(2020).product.toString)
  
  override def solvePart1(input: Input): UIO[String] = solve(2, input)
  override def solvePart2(input: Input): UIO[String] = solve(3, input)
