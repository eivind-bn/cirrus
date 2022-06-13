package cirrus.event

import cirrus.combinator.{PureStream, ModularStream}


abstract class Reporter[I,O] extends EventTree[I,O,Reporter] { parent =>


  final override def dispatch(data: I): Option[O] = dispatch(this, data)


  protected def dispatch[O1](caller: Reporter[I,O1], data: I): Option[O]


  override def prepended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[B,I,DD]): Reporter[B,O] = new Reporter[B,O]{
    override protected def dispatch[O1](caller: Reporter[B, O1], data: B): Option[O] = other match
      case other: Reporter[B,I] => other.dispatch(caller, data).flatMap(data => parent.dispatch(parent, data))
      case other => other.dispatch(data).flatMap(parent.dispatch)
  }


  override def appended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[O,B,DD]): Reporter[I,B] = new Reporter[I,B]{
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[B] = other match
      case other: Reporter[O,B] => parent.dispatch(caller, data).flatMap(data => other.dispatch(other, data))
      case other => parent.dispatch(caller, data).flatMap(data => other.dispatch(data))
  }


  override def scanLeft[B](z: B)(op: (B,O) => B): Reporter[I,B] = new Reporter[I,B]:
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[B] = parent.dispatch(caller, data)
      .map { o => op(z, o) }


  override def filter(pred: O => Boolean): Reporter[I,O] = new Reporter[I,O]:
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filter(pred)


  override def filterNot(pred: O => Boolean): Reporter[I,O] = filter(pred.andThen(!_))


  override def take(n: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filter { _ => i += 1; i < n }
  }


  override def takeWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filter{ o => if flag then flag = p(o); flag }
  }


  override def drop(n: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filterNot { _ => i += 1; i < n }
  }


  override def dropWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filterNot{ o => if flag then flag = p(o); flag }
  }


  override def slice(from: Int, until: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .filter{ _ => i += 1; i >= from && i < until }
  }


  override def map[B](f: O => B): Reporter[I,B] = new Reporter[I,B]:
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[B] = parent.dispatch(caller, data)
      .map(f)


  def flatMap[A, B, DD[_, O] <: PureStream[_, O, DD]](f: O => PureStream[A, B, DD]): Reporter[I,B] = new Reporter[I,B]{
    var dataCapture: Option[B] = None

    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[B] = dataCapture match
      case Some(_) =>
        val temp = dataCapture
        dataCapture = None
        temp

      case None =>
        parent.dispatch(data).foreach{ o =>
          f(o).foreach { b =>
            dataCapture = Some(b)
            caller.dispatch(data)
          }
        }
        dataCapture
  }


  override def collect[B](pf: O ~> B): Reporter[I,B] = new Reporter[I,B]{
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[B] = parent.dispatch(caller, data).collect(pf)
  }


  override def zipWithIndex: Reporter[I,(O,Int)] = new Reporter[I,(O,Int)] {
    var i: Int = -1
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[(O, Int)] = parent.dispatch(caller, data)
      .zip{i += 1; Some(i)}
  }


  override def span(p: O => Boolean): (Reporter[I,O], Reporter[I,O]) = {
    var flag = true

    val left: Reporter[I,O] = new Reporter[I,O]{
      override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
        .filter { o => if flag then flag = p(o); flag }
    }

    val right: Reporter[I,O] = new Reporter[I,O]{
      override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
        .filterNot { o => flag }
    }

    left -> right
  }


  override def splitAt(n: Int): (Reporter[I,O], Reporter[I,O]) = {
    var i = -1

    val left: Reporter[I,O] = new Reporter[I,O]{
      override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
        .filter { o => i += 1; i < n }
    }

    val right: Reporter[I,O] = new Reporter[I,O]{
      override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
        .filterNot { o => i < n }
    }

    left -> right
  }


  override def tapEach[U](f: O => U): Reporter[I,O] = new Reporter[I,O]{
    override protected def dispatch[O1](caller: Reporter[I, O1], data: I): Option[O] = parent.dispatch(caller, data)
      .tapEach(o => f(o))
      .lastOption
  }


  override def foreach(f: O => Unit): Unit = tapEach(f)


}
object Reporter extends EventTree.Factory[Reporter]{

  override def apply[T]: Reporter[T, T] = new Reporter[T,T]:
    override protected def dispatch[O1](caller: Reporter[T, O1], data: T): Option[T] = Some(data)

}