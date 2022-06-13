package cirrus.combinator

trait Stream[I, O, CC[_, O] <: Stream[_, O, CC]]{ parent =>
  export Stream.~>

  def as[DD[_,O] <: Stream[_,O,DD]](factory: Stream.Factory[DD]): DD[I,O] = ???
  
  def foreach(f: O => Unit): Unit

}
object Stream{

  type ~>[-A,+B] = PartialFunction[A,B]

  trait Factory[DD[_,O] <: Stream[_,O,DD]]

  extension [I,O,CC[_,O] <: Stream[_,O,CC]](using ev: Unit =:= O)(stream: Stream[I,O,CC])
    def foreach(f: => Unit): Unit = stream.foreach(_ => f)
  
}
