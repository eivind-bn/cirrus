package event

import scala.collection.mutable.ListBuffer

import scala.concurrent.ExecutionContext

trait Broadcaster[I,O] extends Event[I,O,Broadcaster] { parent =>

//  private class Relay[O] extends Broadcaster[I,O]{
//    override def fireEvent(data: I): None.type = { parent.fireEvent(data); None }
//  }


  private val listeners: ListBuffer[Broadcaster[I,O]] = ListBuffer.empty


  protected def dispatch(data: O): Unit = listeners.foreach{ child => child.dispatch(data) }


  override def prepended[B,DD[_,_]](other: Event[B, I, DD]): Broadcaster[B, O] = new Broadcaster[B,O] {

    override def fireEvent(data: B): Option[O] = other.fireEvent(data).flatMap(parent.fireEvent)
  }


  override def appended[B,DD[_,_]](other: Event[O, B, DD]): Broadcaster[I, B] = new Broadcaster[I,B] {
    parent.foreach{ o =>
      other.fireEvent(o)
    }
    override def fireEvent(data: I): Option[B] = parent.fireEvent(data).flatMap(other.fireEvent)
  }


  override def scanLeft[B](z: B)(op: (B, O) => B): Broadcaster[I, B] = new Broadcaster[I,B] {
    parent.foreach{ o =>
      dispatch(op(z,o))
    }

    override def fireEvent(data: I): Option[B] = parent.fireEvent(data).map(o => op(z,o))
  }


  override def filter(pred: O => Boolean): Broadcaster[I,O] = new Broadcaster[I,O] {
    parent.foreach{ o =>
      if pred(o) then dispatch(o)
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data).filter(pred)
  }


  override def filterNot(pred: O => Boolean): Broadcaster[I, O] = filter(pred.andThen(!_))


  override def take(n: Int): Broadcaster[I,O] = new Broadcaster[I,O] {
    var i: Int = 0
    parent.foreach{ o =>
      if i < n then {
        i += 1
        dispatch(o)
      }
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
  }


  override def takeWhile(p: O => Boolean): Broadcaster[I,O] = new Broadcaster[I,O] {
    var flag = true
    parent.foreach{ o =>
      if flag then flag = p(o)
      if flag then dispatch(o)
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
  }


  override def drop(n: Int): Broadcaster[I,O] = new Broadcaster[I,O] {
    var i: Int = 0
    parent.foreach{ o =>
      if i >= n then dispatch(o)
      else i += 1
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
  }


  override def dropWhile(p: O => Boolean): Broadcaster[I, O] = new Broadcaster[I,O] {
    var flag = true
    parent.foreach{ o =>
      if flag then flag = p(o)
      if !flag then dispatch(o)
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
  }


  override def slice(from: Int, until: Int): Broadcaster[I,O] = new Broadcaster[I,O] {
    var i: Int = -1
    parent.foreach{ o =>
      if i >= from && i < until then {
        i += 1
        dispatch(o)
      }
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .collect{ case o if i >= from && i < until => i += 1; o }
  }


  override def map[B](f: O => B): Broadcaster[I,B] = new Broadcaster[I,B] {
    parent.foreach{ o =>
      dispatch(f(o))
    }
    override def fireEvent(data: I): Option[B] = parent.fireEvent(data).map(f)
  }


  override def flatMap[B,DD[_,_]](f: O => Event[I, B, DD]): Broadcaster[I,B] = new Broadcaster[I,B] {
    parent.foreach{ o =>
      f(o).foreach{ b =>
        dispatch(b)
      }
    }
    override def fireEvent(data: I): Option[B] = parent.fireEvent(data).map(f).flatMap(_.fireEvent(data))
  }


  override def collect[B](pf: PartialFunction[O,B]): Broadcaster[I,B] = new Broadcaster[I,B] {
    parent.foreach{ o =>
      pf.unapply(o).foreach{ b =>
        dispatch(b)
      }
    }
    override def fireEvent(data: I): Option[B] = parent.fireEvent(data).collect(pf)
  }


  override def zipWithIndex: Broadcaster[I,(O,Int)] = new Broadcaster[I,(O,Int)] {
    var i: Int = -1
    parent.foreach{ o =>
      i += 1
      dispatch(o -> i)
    }
    override def fireEvent(data: I): Option[(O, Int)] = parent.fireEvent(data).map{ o => i += 1; (o,i)}
  }


  override def span(p: O => Boolean): (Broadcaster[I,O], Broadcaster[I,O]) = {

    val (left, right) = new Broadcaster[I,O] -> new Broadcaster[I,O]
    var flag = true

    parent.foreach{ o =>
      if flag then flag = p(o)

      if flag then left.dispatch(o)
      else right.dispatch(o)
    }

    left -> right
  }


  override def splitAt(n: Int): (Broadcaster[I, O], Broadcaster[I, O]) = {

    val (left, right) = new Relay[O] -> new Relay[O]
    var i = 0

    parent.foreach{ o =>
      if i < n then {
        left.dispatch(o)
        i += 1
      }
      else right.dispatch(o)
    }

    left -> right
  }


  override def tapEach[U](f: O => U): Broadcaster[I,O] = new Broadcaster[I,O] {
    parent.foreach{ o =>
      f(o)
      dispatch(o)
    }
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data).tapEach(f).lastOption
  }


  override def foreach(f: O => Unit): Unit = new Broadcaster[I,O] {
    parent.listeners.addOne(this)
    override def dispatch(data: O): Unit = f(data)
    override def fireEvent(data: I): None.type = { parent.fireEvent(data); None }
  }


}
object Broadcaster{

  class Head[O] extends Broadcaster[O,O]{
    override def fireEvent(data: O): Some[O] = {
      dispatch(data)
      Some(data)
    }
  }

  def apply[O]: Head[O] = new Head[O]

}
