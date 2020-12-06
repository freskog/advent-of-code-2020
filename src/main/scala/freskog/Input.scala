package freskog

import zio.Chunk
import zio.blocking.Blocking
import zio.stream.ZStream

import java.io.IOException

case class Input( raw: ZStream[Any, Nothing, String],
                  asString: () => String,
                  asLines: () => Chunk[String],
                  asInts: () => Chunk[Int])
