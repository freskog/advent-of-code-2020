package freskog.day2

import freskog._
import zio._

object Day2Solution extends BaseSolution:

  override def inputFrom: String = "day2/day2-input.txt"

  val PasswordPattern = """(\d+)-(\d+) (\w): (\w+)""".r

  case class Policy(min:Int, max:Int, letter:Char)
  case class Password(policy:Policy, password:String):
    def isValidPart1:Boolean =
      policy.min <= password.count(_ == policy.letter) &&
      policy.max >= password.count(_ == policy.letter)
  
    def isValidPart2:Boolean =
      (password.charAt(policy.min - 1) != policy.letter && password.charAt(policy.max - 1) == policy.letter) ||
      (password.charAt(policy.min - 1) == policy.letter && password.charAt(policy.max - 1) != policy.letter)

  def parsePassword(line:String):Password =
    line match
      case PasswordPattern(min, max, c, pwd) => Password(Policy(min.toInt, max.toInt, c.head), pwd)

  override def solvePart1(input: Input): UIO[String] =
    UIO {
      val (valid, _) = input.asLines().map(parsePassword).partition(_.isValidPart1)
      valid.length.toString
    }

  override def solvePart2(input: Input): UIO[String] =
    UIO {
      val (valid, _) = input.asLines().map(parsePassword).partition(_.isValidPart2)
      valid.length.toString
    }