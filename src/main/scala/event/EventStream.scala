package event

import combinator.PureStream

trait EventStream[I,O,CC[_,O] <: EventStream[_,O,CC]] extends PureStream[I,O,CC] {


  def dispatch(data: I): Option[O]


  def =>:(data: I): Option[O] = dispatch(data)


  def prepended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,I,DD]): CC[B,O]


  def ::[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,I,DD]): CC[B,O] = prepended(other)


  def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): CC[I,B]


  def ~[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): CC[I,B] = appended(other)

}
