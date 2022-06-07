package protocol.http

import protocol.Lexer
import protocol.lang.Ascii

import scala.util.Try

trait Version[SRC <: Ascii] extends Version.Lexeme[SRC]

enum Versions extends Version[Ascii]{
  case `HTTP/0.9` extends Versions
  case `HTTP/1.0` extends Versions
  case `HTTP/1.1` extends Versions
  case `HTTP/2.0` extends Versions
  case `HTTP/3.0` extends Versions
  override val flatten: Ascii = Ascii.decode(productPrefix).get
}

object Version extends Lexer[Ascii]{
  override type CC[SRC <: Ascii] = Version[SRC]

  override def decode[T <: Ascii](rawData: T): Try[Version[T]] = Try{ new Version[T] {
    override val flatten: T = rawData
  }}
}
