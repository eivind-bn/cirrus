package event


trait EventReporter[I,O] extends Event[I,O,EventReporter] { parent =>

  
  def fireEvent(data: I): Option[O]


  def scanLeft[B](z: B)(op: (B, O) => B): EventReporter[I,B] = (data: I) => parent.fireEvent(data)
    .map { o => op(z, o) }


  def filter(pred: O => Boolean): EventReporter[I,O] = (data: I) => parent.fireEvent(data)
    .filter(pred)


  def filterNot(pred: O => Boolean): EventReporter[I,O] = filter(pred.andThen(!_))


  def take(n: Int): EventReporter[I, O] = new EventReporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter { _ => i += 1; i < n }
  }


  def takeWhile(p: O => Boolean): EventReporter[I,O] = new EventReporter[I,O] {
    var flag: Boolean = true
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter{ o => if flag then flag = p(o); flag }
  }


  def drop(n: Int): EventReporter[I,O] = new EventReporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filterNot { _ => i += 1; i < n }
  }


  def dropWhile(p: O => Boolean): EventReporter[I,O] = new EventReporter[I,O] {
    var flag: Boolean = true
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filterNot{ o => if flag then flag = p(o); flag }
  }


  def slice(from: Int, until: Int): EventReporter[I,O] = new EventReporter[I,O] {
    var i: Int = -1
    override def fireEvent(data: I): Option[O] = parent.fireEvent(data)
      .filter{ _ => i += 1; i >= from && i < until }
  }


  def map[B](f: O => B): EventReporter[I,B] = (data: I) => parent.fireEvent(data)
    .map(f)
  
  
  override def flatMap[B](f: O => Event[I, B, _]): EventReporter[I, B] = (data: I) => parent.fireEvent(data)
    .flatMap(o => f(o).fireEvent(data))

  
  def flatten[B](using ev: O <:< EventReporter[I,B]): EventReporter[I,B] = flatMap(ev)


  def collect[B](pf: PartialFunction[O, B]): EventReporter[I,B] = (data: I) => parent.fireEvent(data)
    .collect(pf)


  def zipWithIndex: EventReporter[I,(O,Int)] = new EventReporter[I,(O, Int)] {
    var i: Int = -1
    override def fireEvent(data: I): Option[(O,Int)] = parent.fireEvent(data)
      .zip{i += 1; Some(i)}
  }


  def span(p: O => Boolean): (EventReporter[I,O], EventReporter[I,O]) = takeWhile(p) -> dropWhile(p)


  def splitAt(n: Int): (EventReporter[I,O], EventReporter[I,O]) = {
    var i = -1

    val left: EventReporter[I,O] = (data: I) => parent.fireEvent(data)
      .filter{ o => i += 1; i < n }

    val right: EventReporter[I,O] = (data: I) => parent.fireEvent(data)
      .filterNot{ o => i < n }

    left -> right
  }


  def tapEach[U](f: O => U): EventReporter[I,O] = (data: I) => parent.fireEvent(data)
    .tapEach(o => f(o))
    .lastOption


  def foreach(f: O => Unit): Unit = tapEach(f)
  
  
}
object EventReporter{

  def apply[O]: EventReporter[O,O] = (data: O) => Some(data)

}


