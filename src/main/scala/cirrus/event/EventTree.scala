package cirrus.event

import cirrus.combinator.{ModularStream, PureStream, Stream}

trait EventTree[I,O,CC[_,O] <: EventTree[_,O,CC]] extends ModularStream[I,O,CC] with PureStream[I,O,CC]
object EventTree{
  trait Factory[DD[_,O] <: EventTree[_,O,DD]] extends Stream.Factory[DD] {
    def apply[T]: DD[T,T]
    def of[T]: DD[T,T] = apply[T]
    def like[T](obj: T): DD[T, T] = apply[T]
    def unit: DD[Unit,Unit] = apply[Unit]
    def str: DD[String,String] = apply[String]
    def int: DD[Int,Int] = apply[Int]
    def dec: DD[Double,Double] = apply[Double]
  }
}
