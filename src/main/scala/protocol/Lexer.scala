package protocol
import protocol.lang.{Ascii, US_ASCII}

import scala.util.Try

trait Lexer[IN <: Reader[_]#Stringent[_]] extends Codec[IN] {


  trait Lexeme[SRC <: IN] extends Protocol[SRC] {
    override val flatten: Seq[SRC]
    override final def serialize: Array[Byte] = flatten.flatMap(_.serialize).toArray
    override def toString: String = flatten.toString
  }

}
