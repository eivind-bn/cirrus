package protocol

import event.{Broadcast, Broadcaster, Delegate, Delegator, Report, Reporter}
import pattern.Extractor
import io.Serializable
import protocol.lang.{Ascii, US_ASCII}
import protocol.tcp.TCP

import scala.util.{Failure, Success, Try}


trait Codec[IN] { self =>


  trait Protocol[SRC <: IN] extends Serializable {
    type codec = self.type
    def flatten: SRC
    def codec: self.type = self
  }

  


  type CC[A <: IN] <: Protocol[A]


  def decode[T <: IN](rawData: T): Try[CC[T]]


  def encoderLoop: Broadcaster[IN,Try[CC[IN]]] = Broadcast[IN]
    .map(decode)


  class Observer{
    val encode: Broadcaster[IN, Try[CC[IN]]] = Broadcast[IN].map(self.decode)
    val onReject: Delegator[Throwable] = encode.collect{ case Failure(exception) => exception } :: Delegate[Throwable]
    val onSuccess: Delegator[CC[IN]] = encode.collect{ case Success(value) => value } :: Delegate[CC[IN]]
  }


  def observer: Observer = new Observer








  def greedy = ???


  def reluctant = ???


  def possessive = ???


  object is{
    def unapply[T <: IN,P](arg: T)(using ev: Extractor[Protocol[T],P]): Option[P] = decode(arg) match {
      case Failure(exception) => None
      case Success(value) => ev.unapply(value)
    }
  }


  object of{
    def unapply[T,P](arg: T)(using ev: Extractor[T,P]): Option[P] = ev.unapply(arg)
  }


  def toPartialFunction: PartialFunction[IN,Protocol[IN]] = new PartialFunction[IN,Protocol[IN]] {
    override def isDefinedAt(x: IN): Boolean = self.decode(x).isSuccess
    override def apply(v1: IN): Protocol[IN] = self.decode(v1).get
    override def applyOrElse[A1 <: IN, B1 >: Protocol[IN]](x: A1, default: A1 => B1): B1 = self.decode(x).getOrElse(default(x))
  }

}