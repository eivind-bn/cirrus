package event

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

class Delegator[O] extends Event[O,O,[_,X] =>> Delegator[X]] { delegator =>


  private val worker: mutable.ListBuffer[Delegator[O]] = ListBuffer.empty


  def fireEvent(elem: O): Some[elem.type] = {
    worker.foreach { listener => listener.fireEvent(elem) }
    Some(elem)
  }


  override def prepended[B,DD[_,_]](other: Event[B, O, DD]): Delegator[O] = new Delegator[O]{
    other.foreach{ o =>
      fireEvent(o)
    }
  }


  override def appended[B,DD[_,_]](other: Event[O, B, DD]): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      other.fireEvent(o)
    }
  }



  override def scanLeft[B](z: B)(op: (B, O) => B): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      fireEvent(op(z,o))
    }
  }


  override def filter(p: O => Boolean): Delegator[O] = new Delegator[O]{
    delegator.foreach{ o =>
      if p(o) then fireEvent(o)
    }
  }


  override def filterNot(pred: O => Boolean): Delegator[O] = filter(pred.andThen(!_))


  override def take(n: Int): Delegator[O] = new Delegator[O]{
    var i: Int = 0
    delegator.foreach{ o =>
      if i < n then {
        i += 1
        fireEvent(o)
      }
    }
  }


  override def takeWhile(p: O => Boolean): Delegator[O] = new Delegator[O]{
    var flag = true
    delegator.foreach{ o =>
      if flag then flag = p(o)
      if flag then fireEvent(o)
    }
  }


  override def drop(n: Int): Delegator[O] = new Delegator[O]{
    var i: Int = 0
    delegator.foreach{ o =>
      if i >= n then fireEvent(o)
      else i += 1
    }
  }


  override def dropWhile(p: O => Boolean): Delegator[O] = new Delegator[O]{
    var flag = true
    delegator.foreach{ o =>
      if flag then flag = p(o)
      if !flag then fireEvent(o)
    }
  }


  override def slice(from: Int, until: Int): Delegator[O] = new Delegator[O]{
    var i = 0
    delegator.foreach{ o =>
      if i >= from && i < until then {
        fireEvent(o)
        i += 1
      }
    }
  }


  override def map[B](f: O => B): Delegator[B] = new Delegator[B] {
    delegator.foreach{ o =>
      fireEvent(f(o))
    }
  }


  override def flatMap[B,DD[_,_]](f: O => Event[O, B, DD]): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      f(o).foreach{ b =>
        fireEvent(b)
      }
    }
  }


  override def collect[B](pf: PartialFunction[O, B]): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      pf.unapply(o).foreach{ b =>
        fireEvent(b)
      }
    }
  }


  override def zipWithIndex: Delegator[(O, Int)] = new Delegator[(O, Int)] {
    var i = 0
    delegator.foreach{ o =>
      fireEvent(o -> i)
      i += 1
    }
  }


  override def span(p: O => Boolean): (Delegator[O], Delegator[O]) = {

    val (left, right) = new Delegator[O] -> new Delegator[O]
    var flag = true

    delegator.foreach{ o =>
      if flag then flag = p(o)

      if flag then left.fireEvent(o)
      else right.fireEvent(o)
    }

    left -> right
  }


  override def splitAt(n: Int): (Delegator[O], Delegator[O]) = {

    val (left, right) = new Delegator[O] -> new Delegator[O]
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


  override def tapEach[U](f: O => U): Delegator[O] = new Delegator[O]{
    delegator.foreach{ o =>
      f(o)
      fireEvent(o)
    }
  }


  override def foreach(f: O => Unit): Unit = new Delegator[O] {
    delegator.worker.addOne(this)
    override def fireEvent(elem: O): Some[elem.type] = { f(elem); Some(elem) }
  }

}
object Delegator{

  def apply[O]: Delegator[O] = new Delegator[O]

}

