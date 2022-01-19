package protocol.udp

import pattern.Extractor
import protocol.Codec

import scala.util.Try

trait UDP[T <: Iterable[Byte]] extends UDP.Protocol[T]
object UDP extends Codec[Iterable[Byte]]{
  override type CC[SRC <: Iterable[Byte]] = UDP[SRC]

  override def decode[T <: Iterable[Byte]](rawData: T): Try[UDP[T]] = Try{ new UDP[T] {
    override def flatten: T = ???

    override def serialize: Array[Byte] = ???
  }}
}
