package cirrus.combinator

trait Stream[I, O, CC[_, O] <: Stream[_, O, CC]]{

  type ~>[-A,+B] = PartialFunction[A,B]

  def foreach(f: O => Unit): Unit
  
}
