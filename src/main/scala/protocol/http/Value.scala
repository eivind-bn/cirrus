package protocol.http

import protocol.Lexer
import protocol.lang.Ascii

import scala.util.Try

trait Value[SRC <: Ascii] extends Value.Lexeme[SRC]

enum Values extends Value[Ascii]{
  case `Keep-Alive` extends Values
  case `Close` extends Values
  
  override val flatten: Ascii = Ascii.decode(productPrefix).get
}

object Value extends Lexer[Ascii]{
  override type CC[SRC <: Ascii] = Value[SRC]

  override def decode[T <: Ascii](rawData: T): Try[Value[T]] = Try{ new Value[T] {
    override val flatten: T = rawData
  }}
}
