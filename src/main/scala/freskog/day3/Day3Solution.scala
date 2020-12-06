package freskog.day3

import freskog._
import zio._

object Day3Solution extends BaseSolution:
  override def inputFrom:String = "day3/day3-input.txt"

  enum Tile:
    case Tree extends Tile
    case Open extends Tile
  
  case class Pos(x:Int, y:Int):
    def move(deltaX:Int, deltaY:Int):Pos =
      Pos(x + deltaX, y + deltaY)

  case class World(tiles:Map[Pos,Tile], width:Int, height:Int):
    def tileAt(p:Pos):Tile = tiles(p.copy( x = p.x % width))

  object World:
    def fromString(input:String):World = {
      val (width, height, world) = input.foldLeft((0, 0, Map.empty[Pos, Tile])) {
        case ((_, y, w), '\n') => (    0, y + 1, w)
        case ((x, y, w), '#')  => (x + 1, y    , w.updated(Pos(x, y), Tile.Tree))
        case ((x, y, w), '.')  => (x + 1, y    , w.updated(Pos(x, y), Tile.Open))
        case _ => throw new IllegalArgumentException(input)
      }
      World(world, width, height+1)
    }

  def treesOnPath(found:Long, currentPos:Pos, deltaX:Int, deltaY:Int, world:World):Long =
    if currentPos.y + deltaY > world.height then found
    else
      treesOnPath(
        found + (if world.tileAt(currentPos) == Tile.Tree then 1 else 0),
        currentPos.move(deltaX, deltaY),
        deltaX,
        deltaY,
        world
      )
  
  override def solvePart1(input: Input): UIO[String] =
    UIO(treesOnPath(0, Pos(0,0), 3, 1, World.fromString(input.asString())).toString)
    

  override def solvePart2(input: Input): UIO[String] = 
    UIO {
      val p1 = treesOnPath(0, Pos(0,0), 1, 1, World.fromString(input.asString()))
      val p2 = treesOnPath(0, Pos(0,0), 3, 1, World.fromString(input.asString()))
      val p3 = treesOnPath(0, Pos(0,0), 5, 1, World.fromString(input.asString()))
      val p4 = treesOnPath(0, Pos(0,0), 7, 1, World.fromString(input.asString()))
      val p5 = treesOnPath(0, Pos(0,0), 1, 2, World.fromString(input.asString()))
      (p1 * p2 * p3 * p4 * p5).toString
    }
