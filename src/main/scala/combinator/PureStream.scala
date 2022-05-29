package combinator


trait PureStream[I, O, CC[_, O] <: PureStream[_, O, CC]] extends Stream[I, O, CC] { pure =>


  def scanLeft[B](z: B)(op: (B, O) => B): CC[I, B]


  def filter(pred: O => Boolean): CC[I, O]


  def filterNot(pred: O => Boolean): CC[I, O]


  def take(n: Int): CC[I, O]


  def takeWhile(p: O => Boolean): CC[I, O]


  def drop(n: Int): CC[I, O]


  def dropWhile(p: O => Boolean): CC[I, O]


  def slice(from: Int, until: Int): CC[I, O]


  def map[B](f: O => B): CC[I, B]


  def flatMap[B](f: O => PureStream[I, B, CC]): CC[I, B]


  def flatten[B](using ev: O <:< PureStream[I, B, CC]): CC[I, B] = flatMap(ev)


  def collect[B](pf: O ~> B): CC[I, B]


  def zipWithIndex: CC[I, (O, Int)]


  def span(p: O => Boolean): (CC[I, O], CC[I, O])


  def splitAt(n: Int): (CC[I, O], CC[I, O])


  def tapEach[U](f: O => U): CC[I, O]


  def foreach(f: O => Unit): Unit

}
