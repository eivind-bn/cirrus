package protocol

import protocol.lang.{Ascii, US_ASCII}


import java.nio.ByteBuffer
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder, CodingErrorAction}
import scala.util.Try

trait Reader[IN <: Iterable[Byte]] extends Codec[IN] { self =>

  override type CC[T <: IN] <: Stringent[T]

  protected lazy val encoder: CharsetEncoder = charset
    .newEncoder()
    .onMalformedInput(CodingErrorAction.REPORT)
    .onUnmappableCharacter(CodingErrorAction.REPORT)

  protected lazy val decoder: CharsetDecoder = charset
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPORT)
    .onUnmappableCharacter(CodingErrorAction.REPORT)


  trait Stringent[T <: IN] extends Protocol[T] with Iterable[Char]{
    override def toString: String = decoder.decode(ByteBuffer.wrap(serialize)).toString
  }


  def charset: Charset


  override def decode[T <: IN](rawData: T): Try[CC[T]]


  def decode(rawData: String): Try[CC[IN]]

}
