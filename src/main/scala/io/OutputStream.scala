package io

import event.EventReporter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

trait OutputStream[O] extends EventReporter[O,O] with Output[O] { underlying =>
  override def fireEvent(data: O): Option[O] = ???

  override def apply(v1: O): Unit = ???
  
  override def close(): Unit
  
}
object OutputStream{

  def apply[T]: OutputStream[T] = new OutputStream[T]


}
