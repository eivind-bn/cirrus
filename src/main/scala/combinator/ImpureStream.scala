package combinator

import event.Reporter

trait ImpureStream[I,O,CC[I,O] <: ImpureStream[I,O,CC]] extends Stream[I,O,CC] { impure =>

  def map[B,S](initState: S)(f: (S, O) ~> (S, B)): CC[I, B]

  def stackMap[B,S](initState: S)(f: (S, O) ~> (S, B)): CC[I, Seq[B]]

  def tapEach[U,S](initState: S)(f: (S,O) ~> (S,U)): CC[I,O]
  
}
