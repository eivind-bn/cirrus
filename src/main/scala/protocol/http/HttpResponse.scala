package protocol.http

import protocol.Codec
import protocol.lang.Ascii

import scala.util.Try

trait HttpResponse[SRC <: Iterable[Byte]] extends HttpResponse.Protocol[SRC] {

  val version: Version[Ascii]
  val statusCode: StatusCode[Ascii]
  val properties: Seq[Field[Ascii]]
  val payload: SRC

  override final def serialize: Array[Byte] = ???

}
object HttpResponse extends Codec[Iterable[Byte]]{

  override type CC[SRC <: Iterable[Byte]] = HttpResponse[SRC]

  override def decode[T <: Iterable[Byte]](rawData: T): Try[HttpResponse[T]] = ???
}
