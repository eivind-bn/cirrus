package cirrus.io

import java.io.Closeable


trait Output[T] extends (T => Unit) with AutoCloseable {
  
  override def apply(v1: T): Unit = put(v1)

  def put(data: T): Unit

  def open(): Unit
  
  override def close(): Unit

  def isClosed: Boolean

  def isOpen: Boolean = !isClosed

}
