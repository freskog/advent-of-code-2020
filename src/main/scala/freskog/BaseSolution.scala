package freskog

import zio._
import zio.blocking.Blocking
import zio.stream._

import java.io.IOException
import java.util.concurrent.TimeUnit

trait BaseSolution extends App:
  
  def inputFrom:String
  
  def solvePart1(input:Input):UIO[String] = UIO("")
  def solvePart2(input:Input):UIO[String] = UIO("")

  def inputStream: ZStream[Blocking, Nothing, String] =
    ZStream
      .fromInputStream(this.getClass.getClassLoader.getResourceAsStream(inputFrom))
      .transduce(ZTransducer.utf8Decode)
      .orElseSucceed("")
  
  def readInput: ZIO[Blocking, Nothing, Input] =
    inputStream
      .fold("")(_ ++ _)
      .flatMap(buildInput)
  
  def buildInput(str:String) =
    ZIO.environment[Blocking].map { blocking =>
    Input(
      inputStream.provide(blocking),
      () => str,
      () => Chunk.fromArray(str.split("\n")),
      () => Chunk.fromArray(str.split("\n").map(_.toInt))
    )
  }

  def run(args:List[String]):ZIO[ZEnv, Nothing, ExitCode] = 
    for 
      input  <- readInput
      (timeAndAnswer1) <- solvePart1(input).summarized(clock.currentTime(TimeUnit.MILLISECONDS))((start,end) => end - start)
      _ <- console.putStrLn(s"Part1: ${timeAndAnswer1._1}ms, answer is - ${timeAndAnswer1._2}")
      timeAndAnswer2 <- solvePart2(input).summarized(clock.currentTime(TimeUnit.MILLISECONDS))((start,end) => end - start)
      _ <- console.putStrLn(s"Part2: ${timeAndAnswer2._1}ms, answer is - ${timeAndAnswer2._2}")
    yield 
      ExitCode.success
  
