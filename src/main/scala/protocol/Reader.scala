package protocol

import io.Stream
import protocol.lang.{Ascii, US_ASCII}

import java.nio.ByteBuffer
import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder, CodingErrorAction}
import scala.util.Try



trait Reader[IN <: Byte] extends Codec[IN] { self =>

  override type CC[T <: IN] <: Stringent[T]

  protected lazy val encoder: CharsetEncoder = charset
    .newEncoder()
    .onMalformedInput(CodingErrorAction.REPORT)
    .onUnmappableCharacter(CodingErrorAction.REPORT)

  protected lazy val decoder: CharsetDecoder = charset
    .newDecoder()
    .onMalformedInput(CodingErrorAction.REPORT)
    .onUnmappableCharacter(CodingErrorAction.REPORT)

  

  trait Stringent[T <: IN] extends Protocol[T]{
    def appended(other: CC[IN]): CC[IN] = codec.decode(this.toString ++ other.toString).get
    def ++(other: CC[IN]): CC[IN] = appended(other)
    def prepended(other: CC[IN]): CC[IN] = codec.decode(other.toString ++ this.toString).get
    def ++:(other: CC[IN]): CC[IN] = codec.decode(other.toString ++ this.toString).get
    override def toString: String = decoder.decode(ByteBuffer.wrap(serialize)).toString
  }


  def charset: Charset


  override def decode[T <: IN](rawData: T): Try[CC[T]]


  def decode(rawData: Char): Try[CC[IN]]

}
