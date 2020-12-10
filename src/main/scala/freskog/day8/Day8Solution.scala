package freskog.day8

import freskog.{BaseSolution, Input}
import zio._
import cats.parse.{Numbers, Parser1, Parser => P}

object Day8Solution extends BaseSolution:
  override def inputFrom: String = "day8/day8-input.txt"
  
  val number:Parser1[Int] =
    P.charIn('-', '+').flatMap {
      case '-' => Numbers.nonNegativeIntString.map(_.toInt * -1)
      case '+' => Numbers.nonNegativeIntString.map(_.toInt)
    }
  
  val parseAcc = P.string1("acc ") *> number.map(Instruction.Acc(_))
  val parseNop = P.string1("nop ") *> number.map(Instruction.Nop(_))
  val parseJmp = P.string1("jmp ") *> number.map(Instruction.Jmp(_))
  val parseInst = parseAcc.orElse1(parseJmp).orElse1(parseNop)
  val parseProg:Parser1[Array[Instruction]] = 
    P.rep1Sep(parseInst, 1, P.charIn('\n')).map(_.toList.toArray)
  
  enum Instruction:
    case Nop(n:Int) extends Instruction
    case Acc(n:Int) extends Instruction
    case Jmp(n:Int) extends Instruction
  
    def isAcc:Boolean = this match
      case Acc(_) => true
      case _ => false
    
    def swap:Instruction = this match 
      case Jmp(n) => Nop(n)
      case Nop(n) => Jmp(n)
      case Acc(n) => Acc(n)
  
  case class State(acc:Int, pos:Int, stack:List[Int])
  
  type Machine = Ref[State]
  
  val resetMachine:ZIO[Machine, Nothing, Unit] =
    ZIO.environment[Machine].flatMap(_.set(State(0,0,Nil)))

  def acc(n:Int):ZIO[Machine, Nothing, Unit] =
    ZIO.environment[Machine].flatMap(_.update(m => m.copy( acc = m.acc + n)))
  
  def jmp(n:Int):ZIO[Machine, Nothing, Unit] =
    ZIO.environment[Machine].flatMap(_.update(m => m.copy( pos = m.pos + n)))

  val nextInstruction:ZIO[Machine, Nothing, Unit] =
    jmp(1)
  
  val updateStack:ZIO[Machine, Nothing, Unit] =
    ZIO.environment[Machine].flatMap(_.update(m => m.copy( stack = m.pos :: m.stack)))
  
  val getStack:ZIO[Machine, Nothing, List[Int]] =
    ZIO.environment[Machine].flatMap(_.get.map(_.stack))
  
  val getPos:ZIO[Machine, Nothing, Int] =
    ZIO.environment[Machine].flatMap(_.get.map(_.pos))
  
  val loopDetected:ZIO[Machine, Nothing, Boolean] =
    (getStack zipWith getPos)(_ contains _)
  
  val getAcc:ZIO[Machine, Nothing, Int] =
    ZIO.environment[Machine].flatMap(_.get.map(_.acc))

  def terminated(program:Array[Instruction]):ZIO[Machine, Nothing, Boolean] =
    getPos.map(_ >= program.length)
    
  def runUntilLoop(program:Array[Instruction]):ZIO[Machine, Nothing, Int] =
    ZIO.ifM((loopDetected zipWith terminated(program))(_ || _))(
      getAcc,
      updateStack *> getPos.map(program).flatMap {
        case Instruction.Nop(n) => nextInstruction *> runUntilLoop(program)
        case Instruction.Acc(n) => nextInstruction *> acc(n) *> runUntilLoop(program)
        case Instruction.Jmp(n) => jmp(n) *> runUntilLoop(program)
      }
    )
    
  def findNextJmpOrNop(program:Array[Instruction], pos:Int):Int =
    if program(pos).isAcc then findNextJmpOrNop(program, pos + 1) else pos
  
  def tweak(program:Array[Instruction], pos:Int):Array[Instruction] =
    val copy = program.toArray
    copy.update(pos, program(pos).swap)
    copy
  
  def fixProgram(program:Array[Instruction], pos:Int):ZIO[Machine, Nothing, Int] =
    ZIO.ifM(resetMachine *> runUntilLoop(tweak(program, pos)) *> terminated(program))(
      getAcc,
      fixProgram(program, findNextJmpOrNop(program, pos+1))
    )

  override def solvePart1(input: Input): UIO[String] =
    ZRef.make(State(0,0,Nil)).flatMap( machine => 
      ZIO
        .fromEither(parseProg.parseAll(input.asString()))
        .flatMap(runUntilLoop)
        .provide(machine)
        .fold(err => s"wonky $err", _.toString)
    )

  override def solvePart2(input: Input): UIO[String] =
    ZRef.make(State(0,0,Nil)).flatMap( machine =>
      ZIO
        .fromEither(parseProg.parseAll(input.asString()))
        .flatMap(program => fixProgram(program, findNextJmpOrNop(program, 0)))
        .provide(machine)
        .fold(err => s"wonky $err", _.toString)
    )