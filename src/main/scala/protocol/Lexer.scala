package protocol
import protocol.lang.{Ascii, US_ASCII}

import scala.util.Try

trait Lexer[IN <: Reader[_]#Stringent[_]] extends Codec[IN] {


  trait Lexeme[SRC <: IN] extends Protocol[SRC] {
    override val flatten: SRC
    override final def serialize: Array[Byte] = flatten.serialize
    override def toString: String = flatten.toString
  }

}
