package event


trait Reporter[I,O] extends Event[I,O,Reporter] { parent =>


  def fireEvent(data: I): Option[O]


  override def prepended[B,DD[_,_]](other: Event[B, I, DD]): Reporter[B, O] = (data: B) => other.fireEvent(data)
    .flatMap(parent.fireEvent)


  // TODO: Override to temporary fix AbstractMethodError bug. Presumably a scala-compiler bug.
  override def ::[B, DD[_, _]](other: Event[B, I, DD]): Reporter[B, O] = prepended(other)


  override def appended[B,DD[_,_]](other: Event[O, B, DD]): Reporter[I, B] = (data: I) => parent.fireEvent(data)
    .flatMap(other.fireEvent)


  // TODO: Override to temporary fix AbstractMethodError bug. Presumably a scala-compiler bug.
  override def ~[B, DD[_, _]](other: Event[O, B, DD]): Reporter[I, B] = appended(other)


  override def scanLeft[B](z: B)(op: (B, O) => B): Reporter[I,B] = (data: I) => parent.fireEvent(data)
    .map { o => op(z, o) }


  override def filter(pred: O => Boolean): Reporter[I,O] = (data: I) => parent.fireEvent(data)
    .filter(pred)


  override def filterNot(pred: O => Boolean): Reporter[I,O] = filter(pred.andThen(!_))


  override def take(n: Int): Reporter[I, O] = new Reporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter { _ => i += 1; i < n }
  }


  override def takeWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter{ o => if flag then flag = p(o); flag }
  }


  override def drop(n: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filterNot { _ => i += 1; i < n }
  }


  override def dropWhile(p: O => Boolean): Reporter[I,O] = new Reporter[I,O] {
    var flag: Boolean = true
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filterNot{ o => if flag then flag = p(o); flag }
  }


  override def slice(from: Int, until: Int): Reporter[I,O] = new Reporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter{ _ => i += 1; i >= from && i < until }
  }


  override def map[B](f: O => B): Reporter[I,B] = (data: I) => parent.fireEvent(data)
    .map(f)


  override def flatMap[B,DD[_,_]](f: O => Event[I, B, DD]): Reporter[I, B] = (data: I) => parent.fireEvent(data)
    .flatMap{ o => f(o).fireEvent(data) }


  override def collect[B](pf: PartialFunction[O, B]): Reporter[I,B] = (data: I) => parent.fireEvent(data)
    .collect(pf)


  override def zipWithIndex: Reporter[I,(O,Int)] = new Reporter[I,(O, Int)] {
    var i: Int = -1
    override def fireEvent(data: I): Option[(O,Int)] = parent.fireEvent(data)
      .zip{i += 1; Some(i)}
  }


  override def span(p: O => Boolean): (Reporter[I,O], Reporter[I,O]) = takeWhile(p) -> dropWhile(p)


  override def splitAt(n: Int): (Reporter[I,O], Reporter[I,O]) = {
    var i = -1

    val left: Reporter[I,O] = (data: I) => parent.fireEvent(data)
      .filter{ o => i += 1; i < n }

    val right: Reporter[I,O] = (data: I) => parent.fireEvent(data)
      .filterNot{ o => i < n }

    left -> right
  }


  override def tapEach[U](f: O => U): Reporter[I,O] = (data: I) => parent.fireEvent(data)
    .tapEach(o => f(o))
    .lastOption


  override def foreach(f: O => Unit): Unit = tapEach(f)


}
object Reporter{

  def apply[O]: Reporter[O,O] = (data: O) => Some(data)

}


