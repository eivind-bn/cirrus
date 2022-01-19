package protocol.lang

import protocol.lang.US_ASCII.decode
import protocol.{Codec, Reader}

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{Charset, StandardCharsets}
import scala.util.Try


trait US_ASCII[T <: Iterable[Byte]] extends US_ASCII.Stringent[T]
object US_ASCII extends Reader[Iterable[Byte]] {

  override type CC[T <: Iterable[Byte]] = US_ASCII[T]


  override def charset: Charset = StandardCharsets.US_ASCII


  override def decode[T <: Iterable[Byte]](rawData: T): Try[US_ASCII[T]] = Try{ new US_ASCII[T] {
    override def iterator: Iterator[Char] = toString.iterator
    override def flatten: T = rawData
    override def serialize: Array[Byte] = flatten.toArray
    override val toString: String = super.toString
  }}


  override def decode(rawData: String): Try[US_ASCII[Iterable[Byte]]] = Try{ new US_ASCII[Iterable[Byte]] {
    override def iterator: Iterator[Char] = rawData.iterator
    override def flatten: Iterable[Byte] = serialize
    override val serialize: Array[Byte] = encoder.encode(CharBuffer.wrap(rawData)).array()
    override def toString: String = rawData
  }}


  extension (sc: StringContext)
    def ascii(args: Ascii*): Ascii = decode(StringContext(sc.parts: _*).s(args.map(_.toString): _*)).get

}
