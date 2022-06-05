package io

import java.io.Closeable


trait Input[T] extends (() => T) with Closeable{

  override def apply(): T = get()
  
  def isClosed: Boolean
  
  def isOpen: Boolean = !isClosed

  override def close(): Unit
  
  def get(): T
  
}
