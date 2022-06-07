package cirrus.combinator

trait ImpureStream[I,O,CC[_,O] <: ImpureStream[_,O,CC]] extends Stream[I,O,CC] { impure =>

  def map[B,S](initState: S)(f: (S, O) ~> (S, B)): CC[I, B]

  def stackMap[B,S](initState: S)(f: (S, O) ~> (S, B)): CC[I, Seq[B]]

  def tapEach[U,S](initState: S)(f: (S,O) ~> (S,U)): CC[I,O]
  
}
