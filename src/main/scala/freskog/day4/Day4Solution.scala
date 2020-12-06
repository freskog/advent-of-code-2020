package freskog.day4

import freskog._
import zio._
import zio.blocking._
import zio.stream._

import scala.util.Try

object Day4Solution extends BaseSolution:
  override def inputFrom: String = "day4/day4-input.txt"

  enum PropType:
    case Color(rgb:String) extends PropType
    case Centimeters(length:Int) extends PropType
    case Inches(length:Int) extends PropType
    case Year(n:Int) extends PropType
    case Id(id:String) extends PropType
  
  enum Prop:
    case BirthYear(byr: PropType.Year) extends Prop     
    case IssueYear(iyr: PropType.Year) extends Prop
    case ExpirationYear(eyr: PropType.Year) extends Prop
    case Height(hgt: PropType) extends Prop
    case HairColor(hcl: PropType.Color) extends Prop
    case EyeColor(ecl: PropType.Color) extends Prop
    case PassportId(pid: PropType.Id) extends Prop
    case CountryId(cid: PropType.Id) extends Prop
    case Invalid(input:String) extends Prop
  
  case class Entry(data:Map[String, Prop]):
    import freskog.day4.Day4Solution.PropType._
    import freskog.day4.Day4Solution.Prop._
    def isValid = 
      data.keys.toSet.intersect(Set("byr","iyr","eyr","hgt","ecl","hcl","pid")).size == 7
      
    def validEntries =
      isValid && data.keys.forall(validate)
      
    def validate(key:String):Boolean = data.get(key).map {
      case BirthYear(Year(yr)) => 1920 <= yr && yr <= 2002
      case IssueYear(Year(yr)) => 2010 <= yr && yr <= 2020
      case ExpirationYear(Year(yr)) => 2020 <= yr && yr <= 2030
      case Height(Centimeters(len)) => 150 <= len && len <= 193
      case Height(Inches(len)) => 59 <= len && len <= 76
      case HairColor(Color(col)) => col.startsWith("#") && Try(Integer.parseInt(col.tail, 16)).isSuccess && col.length == 7
      case EyeColor(Color(col)) => Set("amb","blu", "brn", "gry", "grn", "hzl", "oth").contains(col)
      case PassportId(Id(id)) => id.length == 9 && id.forall(_.isDigit)
      case CountryId(_) => true
      case Invalid(input) => false
    }.getOrElse(false)

  object Entry:
    import Prop._, PropType._
    def decodeProp(input: String): (String, Prop) =
      input.split(":").toList match
        case "byr" :: year   :: Nil => "byr" -> BirthYear(Year(year.toInt))
        case "iyr" :: year   :: Nil => "iyr" -> IssueYear(Year(year.toInt))
        case "eyr" :: year   :: Nil => "eyr" -> ExpirationYear(Year(year.toInt))
        case "hgt" :: height :: Nil if height.endsWith("cm") => "hgt" -> Height(Centimeters(height.takeWhile(_.isDigit).toInt))
        case "hgt" :: height :: Nil if height.endsWith("in") => "hgt" -> Height(Inches(height.takeWhile(_.isDigit).toInt))
        case "ecl" :: clr    :: Nil => "ecl" -> EyeColor(Color(clr))
        case "hcl" :: clr    :: Nil => "hcl" -> HairColor(Color(clr))
        case "pid" :: id     :: Nil => "pid" -> PassportId(Id(id))
        case "cid" :: id     :: Nil => "cid" -> CountryId(Id(id))
        case key   :: value  :: Nil =>  key  -> Invalid(input)
        
    def fromString(input:String):Entry =
      Entry(input.trim.split("\n| ").foldLeft(Map.empty[String,Prop]) {
        case (acc, pair) => (acc.updated _).tupled(decodeProp(pair))
      })
  
  def parseEntry(input:ZStream[Any, Nothing, String]) =
    input
      .transduce(ZTransducer.splitOn("\n\n"))
      .map(Entry.fromString)

  override def solvePart1(input: Input): UIO[String] =
    parseEntry(input.raw).filter(_.isValid).runCount.map(_.toString)

  override def solvePart2(input: Input): UIO[String] =
    parseEntry(input.raw).filter(_.validEntries).runCount.map(_.toString)
  