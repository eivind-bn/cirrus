package cirrus.io

import cirrus.combinator.PureStream
import cirrus.event.Delegator

import scala.collection.{AbstractIterator, Factory, IterableFactory, mutable}
import scala.concurrent.ExecutionContext

abstract class InputStream[I] extends Input[I] with PureStream[I,I,[_,X] =>> InputStream[X]] { parent =>

  protected val dispatcher: Delegator[I] = Delegator[I]

  class Relay[T](ops: Delegator[I] => Delegator[T]) extends InputStream[T]{
    override protected val dispatcher: Delegator[T] = ops.apply(parent.dispatcher)


    override val onData: Delegator[T] = ???
    override val onOpen: Delegator[Unit] = ???
    override val onClose: Delegator[Unit] = ???
  }

  override def scanLeft[B](z: B)(op: (B, I) => B): InputStream[B] = new Relay[B](_.scanLeft(z)(op))

  override def filter(pred: I => Boolean): InputStream[I] = new Relay[I](_.filter(pred))

  override def filterNot(pred: I => Boolean): InputStream[I] = new Relay[I](_.filterNot(pred))

  override def take(n: Int): InputStream[I] = new Relay[I](_.take(n))

  override def takeWhile(p: I => Boolean): InputStream[I] = new Relay[I](_.takeWhile(p))

  override def drop(n: Int): InputStream[I] = new Relay[I](_.drop(n))

  override def dropWhile(p: I => Boolean): InputStream[I] = new Relay[I](_.dropWhile(p))

  override def slice(from: Int, until: Int): InputStream[I] = new Relay[I](_.slice(from, until))

  override def map[B](f: I => B): InputStream[B] = new Relay[B](_.map(f))

  override def flatMap[A, B, DD[_, O] <: PureStream[_, O, DD]](f: I => PureStream[A, B, DD]): InputStream[B] = new Relay[B](_.flatMap(f))

  override def collect[B](pf: I ~> B): InputStream[B] = new Relay[B](_.collect(pf))

  override def zipWithIndex: InputStream[(I, Int)] = new Relay[(I, Int)](_.zipWithIndex)

  override def span(p: I => Boolean): (InputStream[I], InputStream[I]) = {
    val (leftDispatcher, rightDispatcher) = parent.dispatcher.span(p)

    val left = new Relay[I](identity):
      override protected val dispatcher: Delegator[I] = leftDispatcher

    val right = new Relay[I](identity):
      override protected val dispatcher: Delegator[I] = rightDispatcher

    left -> right
  }

  override def splitAt(n: Int): (InputStream[I], InputStream[I]) = {
    val (leftDispatcher, rightDispatcher) = parent.dispatcher.splitAt(n)

    val left = new Relay[I](identity):
      override protected val dispatcher: Delegator[I] = leftDispatcher

    val right = new Relay[I](identity):
      override protected val dispatcher: Delegator[I] = rightDispatcher

    left -> right
  }

  override def tapEach[U](f: I => U): InputStream[I] = new Relay[I](_.tapEach(f))

  override def foreach(f: I => Unit): Unit = tapEach(f)

}
