package protocol.http

import protocol.Lexer
import protocol.lang.Ascii

import scala.util.Try

trait Method[SRC <: Ascii] extends Method.Lexeme[SRC]

enum Methods extends Method[Ascii]{
  case GET extends Methods
  case HEAD extends Methods
  case POST extends Methods
  case PUT extends Methods
  case DELETE extends Methods
  case CONNECT extends Methods
  case OPTIONS extends Methods
  case TRACE extends Methods
  case PATCH extends Methods
  override val flatten: Ascii = Ascii.decode(productPrefix).get
}

object Method extends Lexer[Ascii]{

  override type CC[SRC <: Ascii] = Method[SRC]

  override def decode[T <: Ascii](rawData: T): Try[Method[T]] = Try{ new Method[T] {
    override val flatten: T = rawData
  }}
}
