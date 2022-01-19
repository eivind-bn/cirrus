package event


import io.Stream


class Delegator[O] extends Event[O,O,[_,X] =>> Delegator[X]] { delegator =>


  protected var _workers: List[Delegator[O]] = List.empty


  protected def detach(): Boolean = false


  def fireEvent(elem: O): Some[elem.type] = {
    _workers.foreach { listener => listener.fireEvent(elem) }
    Some(elem)
  }


  def workers: List[Delegator[O]] = _workers


  override def prepended[B,DD[_,O] <: Event[_,O,DD]](other: Event[B,O,DD]): Delegator[O] = new Delegator[O]{
    other.foreach{ o =>
      fireEvent(o)
    }
  }


  override def appended[B,DD[_,O] <: Event[_,O,DD]](other: Event[O, B, DD]): Delegator[B] = new Delegator[B]{
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
    delegator.foreach{ (node,o) =>
      if i < n then {
        fireEvent(o)
        i += 1
        if i == n then node.detach()
      }
      else node.detach()
    }
  }


  override def takeWhile(p: O => Boolean): Delegator[O] = new Delegator[O]{
    var flag = true
    delegator.foreach{ (node,o) =>
      if flag then flag = p(o)
      if flag then fireEvent(o)
      else node.detach()
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
    if i < until then delegator.foreach{ (node,o) =>
      if i == until - 1 then { fireEvent(o); node.detach() }
      else if i >= from then { fireEvent(o); i += 1 }
      else i += 1
    }
  }


  override def map[B](f: O => B): Delegator[B] = new Delegator[B] {
    delegator.foreach{ o =>
      fireEvent(f(o))
    }
  }


  override def flatMap[B,DD[_,O] <: Stream[_,O,DD]](f: O => Stream[O,B,DD]): Delegator[B] = new Delegator[B]{
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

    val (left, right) = new Delegator[O]{} -> new Delegator[O]{}
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


  override def foreach(f: O => Unit): Unit = foreach{ (node,o) => f(o) }


  private def foreach(f: (Delegator[O],O) => Unit): Unit = new Delegator[O] {
    delegator._workers :+= this
    override protected def detach(): Boolean = { delegator._workers = delegator._workers.diff(Seq(this)); true }
    override def fireEvent(elem: O): Some[elem.type] = { f(this,elem); Some(elem) }
  }


}
object Delegator{

  def apply[O]: Delegator[O] = new Delegator[O]

}

