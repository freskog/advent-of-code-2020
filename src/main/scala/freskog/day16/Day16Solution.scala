package freskog.day16

import cats.data.NonEmptyList
import freskog._
import zio._
import cats.parse.{Numbers, Parser1, Parser => P}

object Day16Solution extends BaseSolution:
  override def inputFrom: String = "day16/day16-input.txt"

  case class Rule(name:String, lower:Range, upper:Range):
    def valid(value:BigInt):Boolean =
     lower.contains(value) || upper.contains(value)
      
  case class Ticket(fields:List[BigInt]):
    
    def fieldAt(idx:Int):BigInt =
      fields(idx)

    def isValid(rules:List[Rule]):Boolean =
      fields.forall( v => rules.exists(_.valid(v)))

    def invalidFields(rules:List[Rule]):List[BigInt] =
      fields.filter( v => rules.forall( rule => !rule.valid(v)))
  
  case class State(rules:List[Rule], myTicket:Ticket, nearByTickets:List[Ticket])
  
  val range: Parser1[Range] = 
    (Numbers.bigInt ~ P.string1("-") ~ Numbers.bigInt).map {
      case low <*> _ <*> high => Range.inclusive(low.toInt, high.toInt)
    }
  
  val rule: Parser1[Rule] = 
    (P.charsWhile1(c => c != ':' && c != '\n') ~ P.string1(": ") ~ range ~ P.string1(" or ") ~ range).map {
      case name <*> _ <*> lower <*> _ <*> upper => Rule(name, lower, upper)
    }

  val rules:P[List[Rule]] =
    P.repSep(rule, 1, P.char('\n'))

  val ticket:Parser1[Ticket] =
    P.rep1Sep(Numbers.bigInt, 1, P.char(',')).map(values => Ticket(values.toList))

  val myTicket:Parser1[Ticket] =
    (P.string1("your ticket:\n") *> ticket)
    
  val nearByTickets:Parser1[List[Ticket]] =
    P.string1("nearby tickets:\n") *> P.repSep(ticket, 1, P.char('\n'))

  val state:P[State] =
    (rules ~ P.string1("\n\n") ~ myTicket ~ P.string1("\n\n") ~ nearByTickets).map {
      case rules <*> _ <*> myTicket <*> _ <*> nearByTickets => State(rules, myTicket, nearByTickets)
    }
    
  def ruleToColumns(rules:List[Rule], tickets:List[Ticket]):Map[Rule, List[Int]] =
    val columnsWithIdx = tickets.map(_.fields).transpose.zipWithIndex  
    rules.foldLeft(Map.empty[Rule,List[Int]]) {
      case (acc, r) => 
        val matchingIdxes = columnsWithIdx.collect { case (col, idx) if col.forall(r.valid) => idx }
        acc.updated(r, matchingIdxes)
    } 
  
  def resolveRules(rules:Map[Rule, List[Int]]):Map[Rule, List[Int]] =
    if rules.values.forall(_.size == 1) then rules
    else resolveRules(rules.filter(_._2.size == 1).foldLeft(rules) {
     case (acc, (r, idx :: _)) => 
       acc.collect { 
         case (r1, idxes) if idxes.contains(idx) && idxes.length > 1 => (r1, idxes.filterNot(_ == idx))
         case (r1, idxes) => (r1, idxes)
       }
    })
    
  def departureValues(key:Map[Rule, List[Int]], ticket:Ticket):List[BigInt] =
    key.keys.filter(_.name.startsWith("departure")).map(name => ticket.fieldAt(key(name).head)).toList
  
  override def solvePart1(input:Input):UIO[String] = 
    state.parseAll(input.asString()) match {
      case Right(state) => UIO(state.nearByTickets.flatMap(_.invalidFields(state.rules)).sum.toString)
      case Left(err) => ZIO.dieMessage(err.toString)
    }

  override def solvePart2(input:Input):UIO[String] =
    state.parseAll(input.asString()) match {
      case Right(state) => 
        UIO(state.nearByTickets.filter(_.isValid(state.rules)))
          .map(ruleToColumns(state.rules, _))
          .map(resolveRules)
          .map(departureValues(_, state.myTicket))
          .map(_.product.toString)
    }