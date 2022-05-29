package event

import combinator.PureStream


class Delegator[O] extends EventStream[O,O,[_,X] =>> Delegator[X]] { delegator =>


  protected var relays: List[Relay] = List.empty

  protected trait Relay{
    delegator.relays :+= this
    def detach(): Unit = delegator.relays = delegator.relays.diff(Seq(this))
    def process(data: O): Unit
  }

  override def dispatch(data: O): Option[data.type] = {
    relays.foreach{ relay => relay.process(data) }
    Some(data)
  }

  override def prepended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,O,DD]): Delegator[O] = new Delegator[O]{
    other.foreach{ o =>
      dispatch(o)
    }
  }


  override def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O, B, DD]): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      other.dispatch(o)
    }
  }


  override def scanLeft[B](z: B)(op: (B, O) => B): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      dispatch(op(z,o))
    }
  }


  override def filter(p: O => Boolean): Delegator[O] = new Delegator[O]{
    delegator.foreach{ o =>
      if p(o) then dispatch(o)
    }
  }


  override def filterNot(pred: O => Boolean): Delegator[O] = filter(pred.andThen(!_))


  override def take(n: Int): Delegator[O] = new Delegator[O]{
    var i: Int = 0
    new delegator.Relay{
      override def process(data: O): Unit = {
        if i < n then {
          dispatch(data)
          i += 1
          if i == n then detach()
        }
        else detach()
      }
    }
  }


  override def takeWhile(p: O => Boolean): Delegator[O] = new Delegator[O]{
    var flag = true
    new delegator.Relay{
      override def process(data: O): Unit = {
        if flag then flag = p(data)
        if flag then dispatch(data)
        else detach()
      }
    }
  }


  override def drop(n: Int): Delegator[O] = new Delegator[O]{
    var i: Int = 0
    delegator.foreach{ o =>
      if i >= n then dispatch(o)
      else i += 1
    }
  }


  override def dropWhile(p: O => Boolean): Delegator[O] = new Delegator[O]{
    var flag = true
    delegator.foreach{ o =>
      if flag then flag = p(o)
      if !flag then dispatch(o)
    }
  }


  override def slice(from: Int, until: Int): Delegator[O] = new Delegator[O]{
    var i = 0
    if i < until then new delegator.Relay{
      override def process(data: O): Unit = {
        if i == until - 1 then { dispatch(data); detach() }
        else if i >= from then { dispatch(data); i += 1 }
        else i += 1
      }
    }
  }


  override def map[B](f: O => B): Delegator[B] = new Delegator[B] {
    delegator.foreach{ o =>
      dispatch(f(o))
    }
  }


  override def flatMap[B](f: O => PureStream[O,B,[_,X] =>> Delegator[X]]): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      f(o).foreach{ b =>
        dispatch(b)
      }
    }
  }


  override def collect[B](pf: O ~> B): Delegator[B] = new Delegator[B]{
    delegator.foreach{ o =>
      pf.unapply(o).foreach{ b =>
        dispatch(b)
      }
    }
  }

  override def zipWithIndex: Delegator[(O, Int)] = new Delegator[(O, Int)] {
    var i = 0
    delegator.foreach{ o =>
      dispatch(o -> i)
      i += 1
    }
  }


  override def span(p: O => Boolean): (Delegator[O], Delegator[O]) = {

    val (left, right) = Delegator[O] -> Delegator[O]
    var flag = true

    delegator.foreach{ o =>
      if flag then flag = p(o)

      if flag then left.dispatch(o)
      else right.dispatch(o)
    }

    left -> right
  }


  override def splitAt(n: Int): (Delegator[O], Delegator[O]) = {

    val (left, right) = Delegator[O] -> Delegator[O]
    var i = 0

    delegator.foreach{ o =>
      if i < n then {
        left.dispatch(o)
        i += 1
      }
      else right.dispatch(o)
    }

    left -> right
  }


  override def tapEach[U](f: O => U): Delegator[O] = new Delegator[O]{
    new delegator.Relay:
      override def process(data: O): Unit = {
        f(data)
        dispatch(data)
      }
  }


  override def foreach(f: O => Unit): Unit = tapEach(f)

}
object Delegator{

  def apply[O]: Delegator[O] = new Delegator[O]

}

