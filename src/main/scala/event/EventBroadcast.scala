package event

import scala.collection.mutable.ListBuffer

trait EventBroadcast[I,O] extends Event[I,O,EventBroadcast] { parent =>

  private class Relay[O] extends EventBroadcast[I,O]{
    override def fireEvent(data: I): None.type = { parent.fireEvent(data); None }
  }


  private val listeners: ListBuffer[Relay[O]] = ListBuffer.empty


  protected def dispatch(data: O): Unit = listeners.foreach{ child => child.dispatch(data) }


  override def scanLeft[B](z: B)(op: (B, O) => B): EventBroadcast[I, B] = new Relay[B] {
    parent.foreach{ o =>
      dispatch(op(z,o))
    }
  }


  override def filter(pred: O => Boolean): EventBroadcast[I, O] = new Relay[O] {
    parent.foreach{ o =>
      if pred(o) then dispatch(o)
    }
  }


  override def filterNot(pred: O => Boolean): EventBroadcast[I, O] = filter(pred.andThen(!_))


  override def take(n: Int): EventBroadcast[I, O] = new Relay[O] {
    var i: Int = 0
    parent.foreach{ o =>
      if i < n then {
        i += 1
        dispatch(o)
      }
    }
  }


  override def takeWhile(p: O => Boolean): EventBroadcast[I, O] = new Relay[O] {
    var flag = true
    parent.foreach{ o =>
      if flag then flag = p(o)
      if flag then dispatch(o)
    }
  }


  override def drop(n: Int): EventBroadcast[I, O] = new Relay[O] {
    var i: Int = 0
    parent.foreach{ o =>
      if i >= n then dispatch(o)
      else i += 1
    }
  }


  override def dropWhile(p: O => Boolean): EventBroadcast[I, O] = new Relay[O] {
    var flag = true
    parent.foreach{ o =>
      if flag then flag = p(o)
      if !flag then dispatch(o)
    }
  }


  override def slice(from: Int, until: Int): EventBroadcast[I, O] = new Relay[O] {
    var i = 0
    parent.foreach{ o =>
      if i >= from && i < until then {
        dispatch(o)
        i += 1
      }
    }
  }


  override def map[B](f: O => B): EventBroadcast[I, B] = new Relay[B] {
    parent.foreach{ o =>
      dispatch(f(o))
    }
  }


  override def flatMap[B](f: O => Event[I, B, _]): EventBroadcast[I, B] = new Relay[B] {
    parent.foreach{ o => 
      f(o).foreach{ b => 
        dispatch(b)
      }
    }
  }


  override def collect[B](pf: PartialFunction[O, B]): EventBroadcast[I, B] = new Relay[B] {
    parent.foreach{ o => 
      pf.unapply(o).foreach{ b => 
        dispatch(b)
      }
    }
  }


  override def zipWithIndex: EventBroadcast[I, (O, Int)] = new Relay[(O, Int)] {
    var i = 0
    parent.foreach{ o =>
      dispatch(o -> i)
      i += 1
    }
  }


  override def span(p: O => Boolean): (EventBroadcast[I, O], EventBroadcast[I, O]) = {

    val (left, right) = new Relay[O] -> new Relay[O]
    var flag = true

    parent.foreach{ o =>
      if flag then flag = p(o)

      if flag then left.dispatch(o)
      else right.dispatch(o)
    }

    left -> right
  }


  override def splitAt(n: Int): (EventBroadcast[I, O], EventBroadcast[I, O]) = {

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


  override def tapEach[U](f: O => U): EventBroadcast[I, O] = new Relay[O] {
    parent.foreach{ o =>
      f(o)
      dispatch(o)
    }
  }


  def foreach(f: O => Unit): Unit = new Relay[O] {
    parent.listeners.addOne(this)
    override def dispatch(data: O): Unit = f(data)
  }


}
object EventBroadcast{

  class Head[O] extends EventBroadcast[O,O]{
    override def fireEvent(data: O): Some[O] = {
      dispatch(data)
      Some(data)
    }
  }

  def apply[O]: Head[O] = new Head[O]

}
