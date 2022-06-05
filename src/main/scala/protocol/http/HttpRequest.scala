package protocol.http
import event.{Report, Reporter}
import pattern.Extractor
import protocol.Codec
import protocol.http.Fields.{Mutual, Request}
import protocol.http.Methods.GET
import protocol.lang.Ascii
import protocol.lang.US_ASCII.ascii
import automation.Machine

import scala.util.Try

trait HttpRequest[SRC <: Iterable[Byte]] extends HttpRequest.Protocol[SRC] {

  val method: Method[Ascii]
  val path: Ascii
  val version: Version[Ascii]
  val properties: Seq[Field[Ascii]]
  val payload: SRC

  override final def serialize: Array[Byte] = ???
}
object HttpRequest extends Codec[Iterable[Byte]]{

  override type CC[SRC <: Iterable[Byte]] = HttpRequest[SRC]



  enum State{
    case Begin, Transact, End
  }


  import State.*
  override def channel[SRC <: Iterable[Byte]]: Reporter[SRC, HttpRequest[SRC]] =
    Report[SRC] ~ Machine.report.stackMap[SRC,State](Begin){
      case (Begin, value) => ???
      case (Transact, o) => ???
      case (End, o) => ???
    }




}
