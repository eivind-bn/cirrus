package io

import scala.collection.{AbstractIterator, Factory, IterableFactory, mutable}

trait InputStream[I] extends Iterator[I] with Input[I] { self =>

  def underlying: Iterator[I] = new AbstractIterator[I] {
    override def hasNext: Boolean = self.hasNext
    override def next(): I = self.next()
  }

  trait Tail[I1] extends InputStream[I1] {
    override def underlying: Iterator[I1]
    override def hasNext: Boolean = {
      val hasNext = underlying.hasNext
      if !hasNext then close()
      hasNext
    }
    override def next(): I1 = underlying.next()
    override def close(): Unit = self.close()
  }

  given [I1]: Conversion[Iterator[I1], Tail[I1]] = iter => new Tail[I1] {
    override def underlying: Iterator[I1] = iter
  }

  override def close(): Unit

  override def hasNext: Boolean

  override def next(): I

  override def apply(): I = next()

  override def scanLeft[B](z: B)(op: (B, I) => B): InputStream[B] = underlying.scanLeft(z)(op)

  override def filter(p: I => Boolean): InputStream[I] = underlying.filter(p)

  override def filterNot(pred: I => Boolean): InputStream[I] = underlying.filterNot(pred)

  override def take(n: Int): InputStream[I] = underlying.take(n)

  override def takeWhile(p: I => Boolean): InputStream[I] = underlying.takeWhile(p)

  override def drop(n: Int): InputStream[I] = new Tail[I] {
    override lazy val underlying: Iterator[I] = self.underlying.drop(n)
  }

  override def dropWhile(p: I => Boolean): InputStream[I] = underlying.dropWhile(p)

  override def slice(from: Int, until: Int): InputStream[I] = underlying.slice(from, until)

  override def map[B](f: I => B): InputStream[B] = underlying.map(f)

  override def flatMap[B](f: I => IterableOnce[B]): InputStream[B] = underlying.flatMap(f)

  override def flatten[B](implicit asIterable: I => IterableOnce[B]): InputStream[B] = underlying.flatten

  override def collect[B](pf: PartialFunction[I, B]): InputStream[B] = underlying.collect(pf)

  override def zipWithIndex: InputStream[(I, Int)] = underlying.zipWithIndex

  override def span(p: I => Boolean): (InputStream[I], InputStream[I]) = {

    val (left, right): (Iterator[I], Iterator[I]) = underlying.span(p)

    var leftDone, rightDone: Boolean = false

    val leftWrap = new Tail[I] {
      override def underlying: Iterator[I] = left
      override def close(): Unit = if rightDone then super.close() else leftDone = true
    }

    val rightWrap = new Tail[I] {
      override def underlying: Iterator[I] = right
      override def close(): Unit = if leftDone then super.close() else rightDone = true
    }

    leftWrap -> rightWrap
  }
  
  override def tapEach[U](f: I => U): InputStream[I] = underlying.tapEach(f)
  
}
