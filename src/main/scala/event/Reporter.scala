package event

import combinator.PureStream


abstract class Reporter[I,O] extends EventStream[I,O,Reporter] { parent =>


  override def prepended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B, I, DD]): Reporter[B, O] = (data: B) => other.dispatch(data)
    .flatMap(parent.dispatch)


  override def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O, B, DD]): Reporter[I, B] = (data: I) => parent.dispatch(data)
    .flatMap(other.dispatch)


  override def scanLeft[B](z: B)(op: (B, O) => B): Reporter[I,B] = (data: I) => parent.dispatch(data)
    .map { o => op(z, o) }


  override def filter(pred: O => Boolean): Reporter[I,O] = (data: I) => parent.dispatch(data)
    .filter(pred)


  override def filterNot(pred: O => Boolean): Reporter[I,O] = filter(pred.andThen(!_))


  override def take(n: Int): Reporter[I, O] = new Reporter[I,O] {
    var i: Int = -1
    override def dispatch(data: I): Option[O] = parent.dispatch(data)
      .filter { _ => i += 1; i < n }
  }


  override def takeWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override def dispatch(data: I): Option[O] = parent.dispatch(data)
      .filter{ o => if flag then flag = p(o); flag }
  }


  override def drop(n: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override def dispatch(data: I): Option[O] = parent.dispatch(data)
      .filterNot { _ => i += 1; i < n }
  }


  override def dropWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override def dispatch(data: I): Option[O] = parent.dispatch(data)
      .filterNot{ o => if flag then flag = p(o); flag }
  }


  override def slice(from: Int, until: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override def dispatch(data: I): Option[O] = parent.dispatch(data)
      .filter{ _ => i += 1; i >= from && i < until }
  }


  override def map[B](f: O => B): Reporter[I,B] = (data: I) => parent.dispatch(data)
    .map(f)


  override def flatMap[B](f: O => PureStream[I, B, Reporter]): Reporter[I, B] = (data: I) => parent.dispatch(data)
    .map{ o => f(o) }
    .flatMap{
      case event: EventStream[I,B,_] => event.dispatch(data)
    }

  override def collect[B](pf: O ~> B): Reporter[I, B] = (data: I) => parent.dispatch(data)
    .collect(pf)


  override def zipWithIndex: Reporter[I,(O,Int)] = new Reporter[I,(O, Int)] {
    var i: Int = -1
    override def dispatch(data: I): Option[(O,Int)] = parent.dispatch(data)
      .zip{i += 1; Some(i)}
  }


  override def span(p: O => Boolean): (Reporter[I,O], Reporter[I,O]) = {
    var flag = true

    val left: Reporter[I,O] = (data: I) => parent.dispatch(data)
      .filter{ o => if flag then flag = p(o); flag }

    val right: Reporter[I,O] = (data: I) => parent.dispatch(data)
      .filterNot{ o => flag }

    left -> right
  }


  override def splitAt(n: Int): (Reporter[I,O], Reporter[I,O]) = {
    var i = -1

    val left: Reporter[I,O] = (data: I) => parent.dispatch(data)
      .filter{ o => i += 1; i < n }

    val right: Reporter[I,O] = (data: I) => parent.dispatch(data)
      .filterNot{ o => i < n }

    left -> right
  }


  override def tapEach[U](f: O => U): Reporter[I,O] = (data: I) => parent.dispatch(data)
    .tapEach(o => f(o))
    .lastOption


  override def foreach(f: O => Unit): Unit = tapEach(f)


}
object Reporter{

  def apply[O]: Reporter[O,O] = (data: O) => Some(data)

}


