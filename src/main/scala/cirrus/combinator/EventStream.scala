package cirrus.combinator

import cirrus.combinator.PureStream

trait EventStream[I,O,CC[_,O] <: EventStream[_,O,CC]] extends Stream[I,O,CC] {


  def dispatch(data: I): Option[O]


  def =>:(data: I): Option[O] = dispatch(data)


  def prepended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,I,DD]): CC[B,O]


  def ::[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,I,DD]): CC[B,O] = prepended(other)


  def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): CC[I,B]


  def ~[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): CC[I,B] = appended(other)

}
object EventStream{

  trait Factory[CC[_,O] <: EventStream[_,O,CC]]{

    def apply[T]: CC[T,T]

    def of[T]: CC[T,T] = apply[T]

    def like[T](obj: T): CC[T, T] = apply[T]

    def unit: CC[Unit,Unit] = apply[Unit]

    def str: CC[String,String] = apply[String]

    def int: CC[Int,Int] = apply[Int]

    def dec: CC[Double,Double] = apply[Double]

  }

  extension [I,O,CC[_,O] <: EventStream[_,O,CC]](using ev: Unit =:= I)(eventStream: EventStream[I,O,CC])
    def dispatch() = eventStream.dispatch(ev(()))

}
