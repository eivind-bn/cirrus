package event

import combinator.PureStream


abstract class Broadcaster[I,O] extends EventStream[I,O,Broadcaster] { parent =>


  protected val delegator: Delegator[O] = new Delegator[O]


  protected class Relay[O1](transformation: Delegator[O] => Delegator[O1]) extends Broadcaster[I,O1] {

    var dataCapture: Option[O1] = None

    override protected val delegator: Delegator[O1] = transformation.apply(parent.delegator)
      .tapEach{ data => dataCapture = Some(data) }

    override def dispatch(data: I): Option[O1] = {
      dataCapture = None
      parent.dispatch(data)
      dataCapture
    }
  }

  override def prepended[B, DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,I,DD]): Broadcaster[B,O] =
    (data: B) => other.dispatch(data).flatMap { i => parent.dispatch(i) }

  override def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): Broadcaster[I,B] =
    (data: I) => parent.dispatch(data).flatMap { o => other.dispatch(o) }

  override def scanLeft[B](z: B)(op: (B,O) => B): Broadcaster[I,B] = new Relay[B](_.scanLeft(z)(op))

  override def filter(pred: O => Boolean): Broadcaster[I,O] = new Relay[O](_.filter(pred))

  override def filterNot(pred: O => Boolean): Broadcaster[I,O] = new Relay[O](_.filterNot(pred))

  override def take(n: Int): Broadcaster[I,O] = new Relay[O](_.take(n))

  override def takeWhile(p: O => Boolean): Broadcaster[I,O] = new Relay[O](_.takeWhile(p))

  override def drop(n: Int): Broadcaster[I,O] = new Relay[O](_.drop(n))

  override def dropWhile(p: O => Boolean): Broadcaster[I,O] = new Relay[O](_.dropWhile(p))

  override def slice(from: Int, until: Int): Broadcaster[I, O] = new Relay[O](_.slice(from, until))

  override def map[B](f: O => B): Broadcaster[I,B] = new Relay[B](_.map(f))

  override def flatMap[A, B, DD[_,O] <: PureStream[_,O,DD]](f: O => PureStream[A, B, DD]): Broadcaster[I,B] = new Broadcaster[I,B]{

    var dataCapture: Option[B] = None

    parent.delegator.map(f).foreach{ pureStream =>
      pureStream.foreach{ b =>
        dataCapture = Some(b)
        this.delegator.dispatch(b)
      }
    }

    override def dispatch(data: I): Option[B] = {
      dataCapture = None
      parent.dispatch(data)
      dataCapture
    }
  }

  override def collect[B](pf: O ~> B): Broadcaster[I,B] = new Relay[B](_.collect(pf))

  override def zipWithIndex: Broadcaster[I, (O,Int)] = new Relay[(O,Int)](_.zipWithIndex)

  override def span(p: O => Boolean): (Broadcaster[I,O], Broadcaster[I,O]) = {

    val (firstDelegate, secondDelegate) = parent.delegator.span(p)

    val firstBroadcast = new Relay[O](_ => firstDelegate)
    val secondBroadcast = new Relay[O](_ => secondDelegate)

    firstBroadcast -> secondBroadcast
  }

  override def splitAt(n: Int): (Broadcaster[I,O], Broadcaster[I,O]) = {

    val (firstDelegate, secondDelegate) = parent.delegator.splitAt(n)

    val firstBroadcast = new Relay[O](_ => firstDelegate)
    val secondBroadcast = new Relay[O](_ => secondDelegate)

    firstBroadcast -> secondBroadcast
  }

  override def tapEach[U](f: O => U): Broadcaster[I,O] = new Relay[O](_.tapEach(f))

  override def foreach(f: O => Unit): Unit = tapEach(f)

}
object Broadcaster{

  def apply[I]: Broadcaster[I,I] = new Broadcaster[I,I]:
    override def dispatch(data: I): Option[I] = delegator.dispatch(data)

}
