package cirrus.combinator

trait PureStream[I, O, CC[_,O] <: PureStream[_,O,CC]] extends Stream[I, O, CC] { pure =>
  
  def scanLeft[B](z: B)(op: (B, O) => B): CC[I, B]
  
  def filter(pred: O => Boolean): CC[I, O]
  
  def filterNot(pred: O => Boolean): CC[I, O]
  
  def take(n: Int): CC[I, O]
  
  def takeWhile(p: O => Boolean): CC[I, O]
  
  def drop(n: Int): CC[I, O]
  
  def dropWhile(p: O => Boolean): CC[I, O]
  
  def slice(from: Int, until: Int): CC[I, O]
  
  def map[B](f: O => B): CC[I, B]
  
  def flatMap[A, B, DD[_, O] <: PureStream[_, O, DD]](f: O => PureStream[A, B, DD]): CC[I, B]
  
  def flatten[A, B, DD[_, O] <: PureStream[_, O, DD]](using ev: O <:< PureStream[A, B, DD]): CC[I, B] = flatMap(ev)
  
  def collect[B](pf: O ~> B): CC[I, B]
  
  def zipWithIndex: CC[I, (O, Int)]
  
  def span(p: O => Boolean): (CC[I, O], CC[I, O])
  
  def splitAt(n: Int): (CC[I, O], CC[I, O])
  
  def tapEach[U](f: O => U): CC[I, O]

}
object PureStream{

  extension [I,O,CC[_,O] <: PureStream[_,O,CC]](using ev: Unit =:= O)(pureStream: PureStream[I,O,CC])
    def filter(pred: => Boolean): CC[I, O] = pureStream.filter(_ => pred)
    def filterNot(pred: => Boolean): CC[I, O] = pureStream.filterNot(_ => pred)
    def takeWhile(p: => Boolean): CC[I, O] = pureStream.takeWhile(_ => p)
    def dropWhile(p: => Boolean): CC[I, O] = pureStream.dropWhile(_ => p)
    def map[B](f: => B): CC[I, B] = pureStream.map(_ => f)
    def flatMap[A, B, DD[_, O] <: PureStream[_, O, DD]](f: => PureStream[A, B, DD]): CC[I, B] = pureStream.flatMap(_ => f)
    def span(p: => Boolean): (CC[I, O], CC[I, O]) = pureStream.span(_ => p)
    def tapEach[U](f: => U): CC[I, O] = pureStream.tapEach(_ => f)

}