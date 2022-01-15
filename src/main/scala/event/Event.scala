package event

import io.Stream

import scala.concurrent.ExecutionContext

trait Event[I,O,CC[_,O] <: Event[_,O,CC]] extends Stream[I,O,CC] {

  def fireEvent(data: I): Option[O]


  def =>:(data: I): Option[O] = fireEvent(data)


  def prepended[B,DD[_,O] <: Event[_,O,DD]](other: Event[B,I,DD]): CC[B,O]


  def ::[B,DD[_,O] <: Event[_,O,DD]](other: Event[B,I,DD]): CC[B,O] = prepended(other)


  def appended[B,DD[_,O] <: Event[_,O,DD]](other: Event[O,B,DD]): CC[I,B]


  def ~[B,DD[_,O] <: Event[_,O,DD]](other: Event[O,B,DD]): CC[I,B] = appended(other)


}
