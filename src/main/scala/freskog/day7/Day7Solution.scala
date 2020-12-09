package freskog.day7

import freskog._
import zio._
import cats.parse.{Parser => P, Parser1, Numbers}

object Day7Solution extends BaseSolution:
  override def inputFrom: String = "day7/day7-input.txt"
  
  case class Color(adj:String,noun:String)
  object ~ {
    def unapply[A,B](ab:(A, B)):Option[(A,B)] = Some(ab)
  }
  
  val whitespace: Parser1[Unit] = 
    P.char(' ')
  
  val comma: Parser1[Unit] = 
    P.char(',')
  
  val word: Parser1[String] = 
    P.charWhere(_.isLetter).rep1.map(_.foldLeft("")(_ + _))
  
  val number: Parser1[Int] = 
    Numbers.signedIntString.map(_.toInt)
  
  val color: Parser1[Color] =
    (word ~ whitespace ~ word).map { case adj ~ _ ~ noun => Color(adj, noun) }
  
  val colorAndQuant: Parser1[(Color, Int)] = 
    (number ~ whitespace ~ color ~ P.string1(" bags").orElse1(P.string1(" bag"))).map { 
      case qty ~ _ ~ col ~ _ => col -> qty
    }
  
  val bags: P[List[(Color, Int)]] =
    P.repSep(colorAndQuant, 1, comma ~ whitespace).orElse(P.string1("no other bags").as(Nil))
  
  val entry: Parser1[(Color, List[(Color, Int)])] = 
    (color ~ P.string1(" bags contain ") ~ bags ).map { 
      case col ~ _ ~ requires => col -> requires 
    } <* P.char('.')

  val allEntries: P[Map[Color, List[(Color, Int)]]] = 
    P.repSep(entry, 1, P.char('\n')).map(_.toMap)
  
  def findRoots(graph:Map[Color,List[(Color,Int)]]):Set[Color] =
    graph.foldLeft(graph.keySet) { case (acc, (_, references)) => acc -- references.map(_._1) }
  
  def dfs(from:List[Color], to:Set[Color], graph:Map[Color, List[(Color,Int)]]):Set[Color] =
    graph.getOrElse(from.head, Nil).foldLeft(to) {
      case (acc, (next, _)) if to contains next => to ++ from
      case (acc, (next, _)) => dfs(next :: from, acc ++ to, graph)
    }
  
  def colorsContainingShinyGold(graph:Map[Color, List[(Color,Int)]]):Int =
    (findRoots(graph)
      .foldLeft(Set.empty[Color]) {
        case (acc, root) => dfs(List(root), Set(Color("shiny","gold")), graph) ++ acc
      } -- Set(Color("shiny","gold"))).size
    
  def sumBagsUnder(color:Color, graph:Map[Color, List[(Color,Int)]]):Int =
    graph.getOrElse(color, Nil).foldLeft(1) {
      case (acc, (next, n)) => acc + (n * sumBagsUnder(next, graph))
    }
      
  def parseEntries(input:String) = 
    ZIO.fromEither(allEntries.parseAll(input))
  
  override def solvePart1(input: Input): UIO[String] =
    parseEntries(input.asString())
      .map(colorsContainingShinyGold)
      .fold(err => s"something went wrong $err", _.toString)

  override def solvePart2(input:Input): UIO[String] =
    parseEntries(input.asString())
      .map(sumBagsUnder(Color("shiny","gold"), _) - 1)
      .fold(err => s"something went wrong $err", _.toString)