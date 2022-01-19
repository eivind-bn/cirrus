package protocol.websocket

import protocol.Codec

import scala.util.Try

trait OpCode[SRC <: Byte] extends OpCode.Protocol[SRC]

enum OpCodes(num: Byte) extends OpCode[Byte]{
  case ContinuationFrame extends OpCodes(0)
  case TextFrame extends OpCodes(1)
  case BinaryFrame extends OpCodes(2)
  case ConnectionClose extends OpCodes(8)
  case Ping extends OpCodes(9)
  case Pong extends OpCodes(10)

  override def flatten: Byte = num
  override def serialize: Array[Byte] = Array(num)
}

object OpCode extends Codec[Byte]{

  override type CC[SRC <: Byte] = OpCode[SRC]

  override def decode[T <: Byte](rawData: T): Try[OpCode[T]] = Try{ new OpCode[T] {
    override def flatten: T = rawData
    override def serialize: Array[Byte] = ???
  }}
}

