package freskog.day10

import freskog._
import zio.UIO

object Day10Solution extends BaseSolution:
  override def inputFrom: String = "day10/day10-input.txt"

  override def solvePart1(input: Input): UIO[String] =
    UIO((0 :: input.asIntPerLine().sorted.toList).sliding(2).foldLeft(Map(1 -> 0, 2 -> 0, 3 -> 1)) {
      case (acc, x1 :: x2 :: Nil) => acc.updated(math.abs(x1-x2), acc(math.abs(x1-x2)) + 1)
    }).map(m => (m(1) * m(3)).toString)


  def update(m:Map[Int,Long], n:Int, count:Long) =
    m.updatedWith(n+1)(_.map(_ + count))
      .updatedWith(n+2)(_.map(_ + count))
      .updatedWith(n+3)(_.map(_ + count))
  
  override def solvePart2(input: Input): UIO[String] = {
    val data = input.asIntPerLine().sorted.toList
    val initial = (0 :: data ::: List(data.last + 3))
    val allZero = initial.map((_,0L)).toMap.updated(0,1L)
    val res = (0 :: data ::: List(data.last + 3)).foldLeft(allZero){
      case (m, n) => update(m, n, m(n)) 
    }
    UIO(res.max._2.toString)
  }