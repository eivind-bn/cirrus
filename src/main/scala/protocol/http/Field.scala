package protocol.http


import protocol.http.Values._
import protocol.lang.Ascii
import protocol.{Lexer, Reader}

import scala.util.Try
import protocol.lang.US_ASCII

sealed trait Field[SRC <: Ascii] extends Field.Lexeme[SRC]{
  val values: Seq[Value[SRC]]
}

object Fields{

  object Request{ export Mutual.* }
  enum Request(override val values: Value[Ascii]*) extends Field[Ascii] {
    case `A-IM`(override val values: Value[Ascii]*) extends Request
    case `Accept`(override val values: Value[Ascii]*) extends Request
    case `Accept-Charset`(override val values: Value[Ascii]*) extends Request
    case `Accept-Datetime`(override val values: Value[Ascii]*) extends Request
    case `Accept-Encoding`(override val values: Value[Ascii]*) extends Request
    case `Accept-Language`(override val values: Value[Ascii]*) extends Request
    case `Access-Control-Request-Method`(override val values: Value[Ascii]*) extends Request
    case `Access-Control-Request-Headers`(override val values: Value[Ascii]*) extends Request
    case `Authorization`(override val values: Value[Ascii]*) extends Request
    case `Cookie`(override val values: Value[Ascii]*) extends Request
    case `Expect`(override val values: Value[Ascii]*) extends Request
    case `Forwarded`(override val values: Value[Ascii]*) extends Request
    case `From`(override val values: Value[Ascii]*) extends Request
    case `Host`(override val values: Value[Ascii]*) extends Request
    case `HTTP2-Settings`(override val values: Value[Ascii]*) extends Request
    case `If-Match`(override val values: Value[Ascii]*) extends Request
    case `If-Modified-Since`(override val values: Value[Ascii]*) extends Request
    case `If-None-Match`(override val values: Value[Ascii]*) extends Request
    case `If-Range`(override val values: Value[Ascii]*) extends Request
    case `If-Unmodified-Since`(override val values: Value[Ascii]*) extends Request
    case `Max-Forwards`(override val values: Value[Ascii]*) extends Request
    case `Origin`(override val values: Value[Ascii]*) extends Request
    case `Prefer`(override val values: Value[Ascii]*) extends Request
    case `Proxy-Authorization`(override val values: Value[Ascii]*) extends Request
    case `Range`(override val values: Value[Ascii]*) extends Request
    case `Referer`(override val values: Value[Ascii]*) extends Request
    case `TE`(override val values: Value[Ascii]*) extends Request
    case `User-Agent`(override val values: Value[Ascii]*) extends Request
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }

  object Response{ export Mutual.* }
  enum Response(val values: Value[Ascii]*) extends Field[Ascii] {
    case `Accept-CH`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Allow-Origin`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Allow-Credentials`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Expose-Headers`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Max-Age`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Allow-Methods`(override val values: Value[Ascii]*) extends Response
    case `Access-Control-Allow-Headers`(override val values: Value[Ascii]*) extends Response
    case `Accept-Patch`(override val values: Value[Ascii]*) extends Response
    case `Accept-Ranges`(override val values: Value[Ascii]*) extends Response
    case `Age`(override val values: Value[Ascii]*) extends Response
    case `Allow`(override val values: Value[Ascii]*) extends Response
    case `Alt-Svc`(override val values: Value[Ascii]*) extends Response
    case `Content-Disposition`(override val values: Value[Ascii]*) extends Response
    case `Content-Language`(override val values: Value[Ascii]*) extends Response
    case `Content-Location`(override val values: Value[Ascii]*) extends Response
    case `Content-Range`(override val values: Value[Ascii]*) extends Response
    case `Delta-Base`(override val values: Value[Ascii]*) extends Response
    case `ETag`(override val values: Value[Ascii]*) extends Response
    case `Expires`(override val values: Value[Ascii]*) extends Response
    case `IM`(override val values: Value[Ascii]*) extends Response
    case `Last-Modified`(override val values: Value[Ascii]*) extends Response
    case `Link`(override val values: Value[Ascii]*) extends Response
    case `Location`(override val values: Value[Ascii]*) extends Response
    case `P3P`(override val values: Value[Ascii]*) extends Response
    case `Preference-Applied`(override val values: Value[Ascii]*) extends Response
    case `Proxy-Authenticate`(override val values: Value[Ascii]*) extends Response
    case `Public-Key-Pins`(override val values: Value[Ascii]*) extends Response
    case `Retry-After`(override val values: Value[Ascii]*) extends Response
    case `Server`(override val values: Value[Ascii]*) extends Response
    case `Set-Cookie`(override val values: Value[Ascii]*) extends Response
    case `Strict-Transport-Security`(override val values: Value[Ascii]*) extends Response
    case `Tk`(override val values: Value[Ascii]*) extends Response
    case `Vary`(override val values: Value[Ascii]*) extends Response
    case `WWW-Authenticate`(override val values: Value[Ascii]*) extends Response
    case `X-Frame-Options`(override val values: Value[Ascii]*) extends Response
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }

  enum Mutual(val values: Value[Ascii]*) extends Field.Lexeme[Ascii]{
    case `Cache-Control`(override val values: Value[Ascii]*) extends Mutual
    case `Connection`(override val values: Value[Ascii]*) extends Mutual
    case `Content-Encoding`(override val values: Value[Ascii]*) extends Mutual
    case `Content-Length`(override val values: Value[Ascii]*) extends Mutual
    case `Content-MD5`(override val values: Value[Ascii]*) extends Mutual
    case `Content-Type`(override val values: Value[Ascii]*) extends Mutual
    case `Date`(override val values: Value[Ascii]*) extends Mutual
    case `Pragma`(override val values: Value[Ascii]*) extends Mutual
    case `Trailer`(override val values: Value[Ascii]*) extends Mutual
    case `Transfer-Encoding`(override val values: Value[Ascii]*) extends Mutual
    case `Upgrade`(override val values: Value[Ascii]*) extends Mutual
    case `Via`(override val values: Value[Ascii]*) extends Mutual
    case `Warning`(override val values: Value[Ascii]*) extends Mutual
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }

}
object Field extends Lexer[Ascii]{

  override type CC[SRC <: Ascii] = Field[SRC]

  override def decode[T <: Ascii](rawData: T): Try[Field[T]] = Try{ new Field[T]{
    override val flatten: T = rawData
    override val values: Seq[Value[T]] = ???
  }}

}



