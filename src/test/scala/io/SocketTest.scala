package io

import automation.{FiniteStateMachine, RotaryStateMachine}
import event.{Broadcast, Broadcaster, Delegate, Delegator, Report, Reporter}
import org.scalatest.wordspec.AnyWordSpec

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.util
import java.util.{Timer, TimerTask}
import scala.collection.immutable.Queue
import scala.collection.mutable
import scala.concurrent.Future
import automation.Machine

class SocketTest  extends AnyWordSpec{

  sealed trait Foo
  case object Bar extends Foo
  case object Baz extends Foo
  case object Jo extends Foo


  val x: Reporter[Int, Seq[Int]] = Report[Int] ~ Machine.report[Int].stackMap(24){
    case (i, i1) => i -> i1
  } ~ Report[Seq[Int]].tapEach(println)



  (0 to 10000).foreach(x.fireEvent)




}
