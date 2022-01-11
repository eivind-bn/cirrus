package event

import scala.concurrent.ExecutionContext

trait Event[I,O,CC[_,_]] {

  def fireEvent(data: I): Option[O]


  def =>:(data: I): Option[O] = fireEvent(data)


  def prepended[B,DD[_,_]](other: Event[B,I,DD]): CC[B,O]


  def ::[B,DD[_,_]](other: Event[B,I,DD]): CC[B,O] = prepended(other)


  def appended[B,DD[_,_]](other: Event[O,B,DD]): CC[I,B]


  def ~[B,DD[_,_]](other: Event[O,B,DD]): CC[I,B] = appended(other)


  def scanLeft[B](z: B)(op: (B, O) => B): CC[I,B]


  def filter(pred: O => Boolean): CC[I,O]


  def filterNot(pred: O => Boolean): CC[I,O]


  def take(n: Int): CC[I, O]


  def takeWhile(p: O => Boolean): CC[I,O]


  def drop(n: Int): CC[I,O]


  def dropWhile(p: O => Boolean): CC[I,O]


  def slice(from: Int, until: Int): CC[I,O]


  def map[B](f: O => B): CC[I,B]


  def flatMap[B,DD[_,_]](f: O => Event[I,B,DD]): CC[I,B]


  def flatten[B,DD[_,_]](using ev: O <:< Event[I,B,DD]): CC[I,B] = flatMap(ev)


  def collect[B](pf: PartialFunction[O, B]): CC[I,B]


  def zipWithIndex: CC[I,(O,Int)]


  def span(p: O => Boolean): (CC[I,O], CC[I,O])


  def splitAt(n: Int): (CC[I,O], CC[I,O])


  def tapEach[U](f: O => U): CC[I,O]


  def foreach(f: O => Unit): Unit

}
