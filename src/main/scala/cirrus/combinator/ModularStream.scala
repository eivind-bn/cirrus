package cirrus.combinator

import cirrus.combinator.PureStream

trait ModularStream[I,O,CC[_,O] <: ModularStream[_,O,CC]] extends Stream[I,O,CC] {

  def dispatch(data: I): Option[O]

  def =>:(data: I): Option[O] = dispatch(data)

  def prepended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[B,I,DD]): CC[B,O]

  def ::[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[B,I,DD]): CC[B,O] = prepended(other)

  def appended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[O,B,DD]): CC[I,B]

  def ~[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[O,B,DD]): CC[I,B] = appended(other)

}
