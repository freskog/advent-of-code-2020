package freskog.day14

import freskog._
import zio._
import cats.parse.{Numbers, Parser1, Parser => P}
import zio.stream.ZTransducer
object Day14Solution extends BaseSolution:
  override def inputFrom: String = "day14/day14-input.txt"

  case class BitMask(mask:String):
    val andValue:Long = 
      java.lang.Long.parseLong(mask.replaceAll("X","1"),2)
      
    val orValue:Long =
      java.lang.Long.parseLong(mask.replaceAll("X", "0"), 2)
    
    def value(value:Long):Long =
      (value & andValue) | orValue
    
    val zeroXs:Long =
      java.lang.Long.parseLong(mask.replaceAll("0","1").replaceAll("X","0"),2)
    
    lazy val v2masks:List[Long] = 
      def go(acc:List[Long], partialMask:List[Char], remaining:List[Char]):List[Long] =
        remaining match
          case Nil => java.lang.Long.parseLong(partialMask.reverse.mkString(""), 2) :: acc
          case 'X' :: unseen => go(acc, '1' :: partialMask, unseen) ++ go(acc, '0' :: partialMask, unseen)
          case digit :: unseen => go(acc, digit :: partialMask, unseen)
      go(Nil, Nil, mask.toList)

    def addresses(input:Long):List[Long] =
      v2masks.map(_ | (input & zeroXs))
  
  
  enum Command:
    case Mask(bitMask: BitMask) extends Command
    case Mem(idx:Long, value:Long) extends Command
  
  object Command:
    val number36bit: P[Long] =
      P.rep(Numbers.digit).map(_.toList.mkString("").toLong)

    val bitMask: Parser1[BitMask] =
      P.rep1(P.charIn("X10"), 36).map(chars => BitMask(chars.toList.mkString))
  
    val maskCommand: Parser1[Command.Mask] =
      P.string1("mask = ") *> bitMask.map(Command.Mask(_))
  
    val memCommand: Parser1[Command.Mem] =
      P.string1("mem[") *> (number36bit ~ (P.string1("] = ") *> number36bit))
        .map { case(idx, v) => Command.Mem(idx, v) }
  
    val command: Parser1[Command] =
      memCommand.orElse1(maskCommand)
  
    def fromString(input:String): UIO[Command] =
        ZIO.fromEither(command.parseAll(input)).orDieWith(e => new IllegalArgumentException(e.toString))
  
  override def solvePart1(input:Input):UIO[String] =
    input.raw.transduce(ZTransducer.splitLines).mapM(Command.fromString(_))
      .fold((BitMask("X" * 36), Map.empty[Long,Long])) {
        case ((_, memory), Command.Mask(newMask)) => (newMask, memory)
        case ((mask, memory), Command.Mem(idx, v)) => (mask, memory.updated(idx, mask.value(v)))
      }.map(_._2.values.sum.toString)
      
  override def solvePart2(input:Input):UIO[String] =
    input.raw.transduce(ZTransducer.splitLines).mapM(Command.fromString)
      .fold((BitMask("0" * 36), Map.empty[Long, Long])) {
        case ((_, memory), Command.Mask(newMask)) => (newMask, memory)
        case ((mask, memory), Command.Mem(idx, v)) => (mask, memory ++ mask.addresses(idx).map(_ -> v))
      }.map(_._2.values.sum.toString)
      
    