package io

import java.io.Closeable


trait Output[T] extends (T => Unit) with Closeable{

  override def apply(v1: T): Unit = put(v1)

  def isClosed: Boolean

  def isOpen: Boolean = !isClosed

  override def close(): Unit

  def put(data: T): Unit

}
