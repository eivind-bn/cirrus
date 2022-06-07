package cirrus.combinator

trait Stream[I, O, CC[_, O] <: Stream[_, O, CC]]{
  export Stream.~>
  
  def foreach(f: O => Unit): Unit
  
}
object Stream{

  type ~>[-A,+B] = PartialFunction[A,B]

  extension [I,O,CC[_,O] <: Stream[_,O,CC]](using ev: Unit =:= O)(stream: Stream[I,O,CC])
    def foreach(f: => Unit): Unit = stream.foreach(_ => f)
  
}
