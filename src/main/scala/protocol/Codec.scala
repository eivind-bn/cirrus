package protocol

import automation.FiniteStateMachine
import event.{Broadcast, Broadcaster, Delegate, Delegator, Report, Reporter}
import pattern.Extractor
import io.Serializable
import protocol.lang.{Ascii, US_ASCII}
import protocol.tcp.TCP

import scala.util.{Failure, Success, Try}


trait Codec[IN] { self =>


  trait Protocol[SRC <: IN] extends Serializable {
    def flatten: Seq[SRC]
    def codec: self.type = self
  }


  type CC[A <: IN] <: Protocol[A]

  def channel[SRC <: IN]: Reporter[SRC,CC[SRC]]



  def greedy = ???


  def reluctant = ???


  def possessive = ???



  object of{
    def unapply[T,P](arg: T)(using ev: Extractor[T,P]): Option[P] = ev.unapply(arg)
  }

}
