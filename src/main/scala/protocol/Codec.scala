package protocol

import pattern.Extractor

import scala.util.Try


trait Codec[A,B[T]] { codec =>













//  def andThen[C[_]](other: Codec[B[A],C]): Codec[A,[X] =>> C[B[X]]] = new Codec[A,[X] =>> C[B[X]]] {
//    override def decode[T <: A](source: Seq[T]): Try[C[B[T]]] = codec.decode(source)
//      .flatMap(o => other.decode(Seq(o.asInstanceOf[B[A]])).asInstanceOf[Try[C[B[T]]]])
//  }





  object ?{
    def unapply[T <: A,P](arg: T)(using ev: Extractor[B[T],P]): Option[P] = ???
  }


  object !{
    def unapply[T <: A,P](arg: B[T])(using ev: Extractor[B[T],P]): Option[P] = ev.unapply(arg)
  }


}
