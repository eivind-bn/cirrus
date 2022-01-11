package io

import event.Reporter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

trait OutputStream[O] extends Reporter[O,O] with Output[O] { underlying =>

  override def fireEvent(data: O): Option[O]

  override def apply(v1: O): Unit = fireEvent(v1)

  override def close(): Unit

}
object OutputStream{

  def apply[T]: OutputStream[T] = ???

}
