package event

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

class EventDelegator[O] extends Event[O,O,[_,X] =>> EventDelegator[X]] { delegator =>


  private val worker: mutable.ListBuffer[EventDelegator[O]] = ListBuffer.empty


  def fireEvent(elem: O): Some[elem.type] = {
    worker.foreach { listener => listener.fireEvent(elem) }
    Some(elem)
  }


  def scanLeft[B](z: B)(op: (B, O) => B): EventDelegator[B] = new EventDelegator[B]{
    delegator.foreach{ o =>
      fireEvent(op(z,o))
    }
  }


  def filter(p: O => Boolean): EventDelegator[O] = new EventDelegator[O]{
    delegator.foreach{ o =>
      if p(o) then fireEvent(o)
    }
  }


  def filterNot(pred: O => Boolean): EventDelegator[O] = filter(pred.andThen(!_))


  def take(n: Int): EventDelegator[O] = new EventDelegator[O]{
    var i: Int = 0
    delegator.foreach{ o =>
      if i < n then {
        i += 1
        fireEvent(o)
      }
    }
  }


  def takeWhile(p: O => Boolean): EventDelegator[O] = new EventDelegator[O]{
    var flag = true
    delegator.foreach{ o =>
      if flag then flag = p(o)
      if flag then fireEvent(o)
    }
  }


  def drop(n: Int): EventDelegator[O] = new EventDelegator[O]{
    var i: Int = 0
    delegator.foreach{ o =>
      if i >= n then fireEvent(o)
      else i += 1
    }
  }


  def dropWhile(p: O => Boolean): EventDelegator[O] = new EventDelegator[O]{
    var flag = true
    delegator.foreach{ o =>
      if flag then flag = p(o)
      if !flag then fireEvent(o)
    }
  }


  def slice(from: Int, until: Int): EventDelegator[O] = new EventDelegator[O]{
    var i = 0
    delegator.foreach{ o =>
      if i >= from && i < until then {
        fireEvent(o)
        i += 1
      }
    }
  }


  def map[B](f: O => B): EventDelegator[B] = new EventDelegator[B] {
    delegator.foreach{ o =>
      fireEvent(f(o))
    }
  }


  override def flatMap[B](f: O => Event[O, B, _]): EventDelegator[B] = new EventDelegator[B]{
    delegator.foreach{ o =>
      f(o).foreach{ b =>
        fireEvent(b)
      }
    }
  }


  def collect[B](pf: PartialFunction[O, B]): EventDelegator[B] = new EventDelegator[B]{
    delegator.foreach{ o =>
      pf.unapply(o).foreach{ b =>
        fireEvent(b)
      }
    }
  }


  def zipWithIndex: EventDelegator[(O, Int)] = new EventDelegator[(O, Int)] {
    var i = 0
    delegator.foreach{ o =>
      fireEvent(o -> i)
      i += 1
    }
  }


  def span(p: O => Boolean): (EventDelegator[O], EventDelegator[O]) = {

    val (left, right) = new EventDelegator[O] -> new EventDelegator[O]
    var flag = true

    delegator.foreach{ o =>
      if flag then flag = p(o)

      if flag then left.fireEvent(o)
      else right.fireEvent(o)
    }

    left -> right
  }


  override def splitAt(n: Int): (EventDelegator[O], EventDelegator[O]) = {

    val (left, right) = new EventDelegator[O] -> new EventDelegator[O]
    var i = 0

    delegator.foreach{ o =>
      if i < n then {
        left.fireEvent(o)
        i += 1
      }
      else right.fireEvent(o)
    }

    left -> right
  }


  def tapEach[U](f: O => U): EventDelegator[O] = new EventDelegator[O]{
    delegator.foreach{ o =>
      f(o)
      fireEvent(o)
    }
  }


  def foreach(f: O => Unit): Unit = new EventDelegator[O] {
    delegator.worker.addOne(this)
    override def fireEvent(elem: O): Some[elem.type] = { f(elem); Some(elem) }
  }

}
object EventDelegator{

  def apply[O]: EventDelegator[O] = new EventDelegator[O]

}

