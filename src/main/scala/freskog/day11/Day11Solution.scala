package freskog.day11

import freskog._
import zio._

object Day11Solution extends BaseSolution:

  override def inputFrom:String = "day11/day11-input.txt"

  enum Tile(letter:Char):
    case EmptySeat extends Tile('L')
    case TakenSeat extends Tile('#')
    case Floor extends Tile('.')

    def isSeat:Boolean = this match
      case TakenSeat | EmptySeat => true
      case _ => false

    def isTaken:Boolean = this match
      case TakenSeat => true
      case _ => false

    def render:String = s"$letter"

  import Tile._
  
  case class Pos(x:Int, y:Int)

  def parseModel(input:String):(Pos,Map[Pos,Tile]) =
    input.foldLeft((Pos(0,0), Map.empty[Pos,Tile])) {
      case ((Pos(x,y), m), '\n') => (Pos(0,y+1),m)
      case ((Pos(x,y), m), '.') => (Pos(x+1,y),m.updated(Pos(x,y), Floor))
      case ((Pos(x,y), m), 'L') => (Pos(x+1,y),m.updated(Pos(x,y), EmptySeat))
      case ((Pos(x,y), m), '#') => (Pos(x+1,y),m.updated(Pos(x,y), TakenSeat))
    }

  def neighbors(p:Pos, m:Map[Pos,Tile]):List[Tile] =
      List(
        m.get(Pos(p.x-1,p.y)), m.get(Pos(p.x-1,p.y-1)), m.get(Pos(p.x, p.y-1)),
        m.get(Pos(p.x+1,p.y-1)), m.get(Pos(p.x+1,p.y)), m.get(Pos(p.x+1, p.y+1)),
        m.get(Pos(p.x,p.y+1)), m.get(Pos(p.x-1,p.y+1))
      ).flatten

  def firstSeatIn(pos:Pos, delta: Pos => Pos, m:Map[Pos,Tile]):Option[Tile] =
    delta(pos) match 
      case p if m.contains(p) && m(p).isSeat => Option(m(p))
      case p if m.contains(p) => firstSeatIn(p, delta, m)
      case p => None
  
  def neighborsPart2(p:Pos, m:Map[Pos,Tile]):List[Tile] =
      val left      = firstSeatIn(p, p => Pos(p.x - 1, p.y    ), m)
      val leftUp    = firstSeatIn(p, p => Pos(p.x - 1, p.y - 1), m)
      val up        = firstSeatIn(p, p => Pos(p.x    , p.y - 1), m)
      val rightUp   = firstSeatIn(p, p => Pos(p.x + 1, p.y - 1), m)
      val right     = firstSeatIn(p, p => Pos(p.x + 1, p.y    ), m)
      val rightDown = firstSeatIn(p, p => Pos(p.x + 1, p.y + 1), m)
      val down      = firstSeatIn(p, p => Pos(p.x    , p.y + 1), m)
      val leftDown  = firstSeatIn(p, p => Pos(p.x - 1, p.y + 1), m)
     
      List(left, leftUp, up, rightUp, right, rightDown, down, leftDown).flatten


  def nextStatePart2(p:Pos, m:Map[Pos, Tile]):Tile =
    (m(p), neighborsPart2(p,m)) match
      case (EmptySeat, adjacent) if !adjacent.contains(TakenSeat) => TakenSeat
      case (TakenSeat, adjacent) if adjacent.filter(_.isTaken).size >= 5 => EmptySeat
      case (tile, _) => tile

  def nextState(p:Pos, m:Map[Pos, Tile]):Tile =
    (m(p), neighbors(p,m)) match
      case (EmptySeat, adjacent) if !adjacent.contains(TakenSeat) => TakenSeat
      case (TakenSeat, adjacent) if adjacent.filter(_.isTaken).size >= 4 => EmptySeat
      case (tile, _) => tile

  def updateStates(m:Map[Pos, Tile]):Map[Pos, Tile] =
    val (updated, res) = m.foldLeft((false, m)) {
      case ((changed, acc), (p, t)) => 
        val next = nextState(p, m)
        (t != next || changed, acc.updated(p, next))
    }
    if updated then updateStates(res) else res


  def updateStatesPart2(m:Map[Pos, Tile], n:Int = 0):Map[Pos, Tile] =
    if n > 100 then throw new IllegalArgumentException()
    val (updated, res) = m.foldLeft((false, m)) {
      case ((changed, acc), (p, t)) => 
        val next = nextStatePart2(p, m)
        (t != next || changed, acc.updated(p, next))
    }
    if updated then updateStatesPart2(res, n + 1) else res
  

  def render(m:Map[Pos,Tile]):String =
    val width = m.maxBy(_._1.x)._1.x
    val height = m.maxBy(_._1.y)._1.y
    val rows = 0 to height
    val cols = 0 to width
    rows.foldLeft("")((rows, no) => 
      rows ++ "\n" ++ cols.foldLeft("")((row, col) => row + m(Pos(col, no)).render)
    )
    

  override def solvePart1(input:Input):UIO[String] =
    UIO(updateStates(parseModel(input.asString())._2).filter(_._2.isTaken).size.toString)

  override def solvePart2(input:Input):UIO[String] =
    UIO(updateStatesPart2(parseModel(input.asString())._2).filter(_._2.isTaken).size.toString)
  