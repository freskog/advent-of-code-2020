package freskog.day12

import freskog._
import zio._
import zio.stream.ZTransducer

object Day12Solution extends BaseSolution:
  override def inputFrom:String = "day12/day12-input.txt"

  val instructionP = """(\w)(\d+)""".r

  case class Location(facing:String, x:Int, y:Int):
    
    val directions = Chunk("W","N","E","S")
    
    def move(direction:String, amt:Int):Location =
      direction match
        case "W" => Location(facing, x-amt, y)
        case "N" => Location(facing, x, y-amt)
        case "E" => Location(facing, x+amt, y)
        case "S" => Location(facing, x, y+amt)
        
    def moveRelative(dx:Int, dy:Int):Location =
      Location(facing, x + dx, y + dy)
    
    def turnLeft(degrees:Int):Location = 
      val newIdx = (directions.indexOf(facing) - (degrees / 90)) % 4
      Location(directions(if newIdx < 0 then directions.length + newIdx else newIdx), x, y)

    def turnRight(degrees:Int):Location =
      val newIdx = (directions.indexOf(facing) + (degrees / 90)) % 4
      Location(directions(newIdx), x, y)
    
    def rotate90(n:Int):Location = 
      if n == 0 then this else Location(facing, -1 * y, x).rotate90(n-1)
    
    def rotateLeft(degrees:Int):Location =
      rotate90((360 - degrees) / 90)
      
    def rotateRight(degrees:Int):Location =
      rotate90(degrees / 90)
    
    def update(instruction:String):Location = 
      instruction match 
        case instructionP("F", n) => move(facing, n.toInt)
        case instructionP("L", n) => turnLeft(n.toInt)
        case instructionP("R", n) => turnRight(n.toInt)
        case instructionP(dir, n) => move(dir, n.toInt)

  case class Boat(location:Location, waypoint:Location):
    def update(instruction:String):Boat =
      instruction match
        case instructionP("F",n) => Boat((0 until n.toInt).foldLeft(location)((l,_) => l.moveRelative(waypoint.x,waypoint.y)), waypoint)
        case instructionP("L",n) => Boat(location, waypoint.rotateLeft(n.toInt))
        case instructionP("R",n) => Boat(location, waypoint.rotateRight(n.toInt))
        case instructionP(dir,n) => Boat(location, waypoint.move(dir, n.toInt))
  
  override def solvePart1(input: Input): UIO[String] =
    input.raw.transduce(ZTransducer.splitLines).fold(Location("E",0,0))(_ update _).map(l => (math.abs(l.x) + math.abs(l.y)).toString)

  override def solvePart2(input:Input):UIO[String] =
    input.raw
      .transduce(ZTransducer.splitLines)
      .fold(Boat(Location("E",0,0),Location("E", 10, -1)))( _ update _)
      .map(_.location)
      .map(l => (math.abs(l.x) + math.abs(l.y)).toString)