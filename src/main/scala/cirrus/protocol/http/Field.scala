package protocol.http


import protocol.http.Values._
import protocol.lang.Ascii
import protocol.{Lexer, Reader}

import scala.util.Try
import protocol.lang.US_ASCII
import protocol.lang.US_ASCII.ascii

sealed trait Field[SRC <: Ascii] extends Field.Lexeme[SRC]{
  val values: Seq[Value[SRC]]
}

object Fields{

  def flattenAll(field: String, values: Seq[Value[Ascii]]): Ascii = ???

  object Request{ export Mutual.* }
  enum Request(override val values: Value[Ascii]*) extends Field[Ascii] {
    case `A-IM`(override val values: Value[Ascii]*) extends Request(values*)
    case `Accept`(override val values: Value[Ascii]*) extends Request(values*)
    case `Accept-Charset`(override val values: Value[Ascii]*) extends Request(values*)
    case `Accept-Datetime`(override val values: Value[Ascii]*) extends Request(values*)
    case `Accept-Encoding`(override val values: Value[Ascii]*) extends Request(values*)
    case `Accept-Language`(override val values: Value[Ascii]*) extends Request(values*)
    case `Access-Control-Request-Method`(override val values: Value[Ascii]*) extends Request(values*)
    case `Access-Control-Request-Headers`(override val values: Value[Ascii]*) extends Request(values*)
    case `Authorization`(override val values: Value[Ascii]*) extends Request(values*)
    case `Cookie`(override val values: Value[Ascii]*) extends Request(values*)
    case `Expect`(override val values: Value[Ascii]*) extends Request(values*)
    case `Forwarded`(override val values: Value[Ascii]*) extends Request(values*)
    case `From`(override val values: Value[Ascii]*) extends Request(values*)
    case `Host`(override val values: Value[Ascii]*) extends Request(values*)
    case `HTTP2-Settings`(override val values: Value[Ascii]*) extends Request(values*)
    case `If-Match`(override val values: Value[Ascii]*) extends Request(values*)
    case `If-Modified-Since`(override val values: Value[Ascii]*) extends Request(values*)
    case `If-None-Match`(override val values: Value[Ascii]*) extends Request(values*)
    case `If-Range`(override val values: Value[Ascii]*) extends Request(values*)
    case `If-Unmodified-Since`(override val values: Value[Ascii]*) extends Request(values*)
    case `Max-Forwards`(override val values: Value[Ascii]*) extends Request(values*)
    case `Origin`(override val values: Value[Ascii]*) extends Request(values*)
    case `Prefer`(override val values: Value[Ascii]*) extends Request(values*)
    case `Proxy-Authorization`(override val values: Value[Ascii]*) extends Request(values*)
    case `Range`(override val values: Value[Ascii]*) extends Request(values*)
    case `Referer`(override val values: Value[Ascii]*) extends Request(values*)
    case `TE`(override val values: Value[Ascii]*) extends Request(values*)
    case `User-Agent`(override val values: Value[Ascii]*) extends Request(values*)
    override val flatten: Ascii = flattenAll(productPrefix, values)
  }

  object Response{ export Mutual.* }
  enum Response(val values: Value[Ascii]*) extends Field[Ascii] {
    case `Accept-CH`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Allow-Origin`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Allow-Credentials`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Expose-Headers`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Max-Age`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Allow-Methods`(override val values: Value[Ascii]*) extends Response(values*)
    case `Access-Control-Allow-Headers`(override val values: Value[Ascii]*) extends Response(values*)
    case `Accept-Patch`(override val values: Value[Ascii]*) extends Response(values*)
    case `Accept-Ranges`(override val values: Value[Ascii]*) extends Response(values*)
    case `Age`(override val values: Value[Ascii]*) extends Response(values*)
    case `Allow`(override val values: Value[Ascii]*) extends Response(values*)
    case `Alt-Svc`(override val values: Value[Ascii]*) extends Response(values*)
    case `Content-Disposition`(override val values: Value[Ascii]*) extends Response(values*)
    case `Content-Language`(override val values: Value[Ascii]*) extends Response(values*)
    case `Content-Location`(override val values: Value[Ascii]*) extends Response(values*)
    case `Content-Range`(override val values: Value[Ascii]*) extends Response(values*)
    case `Delta-Base`(override val values: Value[Ascii]*) extends Response(values*)
    case `ETag`(override val values: Value[Ascii]*) extends Response(values*)
    case `Expires`(override val values: Value[Ascii]*) extends Response(values*)
    case `IM`(override val values: Value[Ascii]*) extends Response(values*)
    case `Last-Modified`(override val values: Value[Ascii]*) extends Response(values*)
    case `Link`(override val values: Value[Ascii]*) extends Response(values*)
    case `Location`(override val values: Value[Ascii]*) extends Response(values*)
    case `P3P`(override val values: Value[Ascii]*) extends Response(values*)
    case `Preference-Applied`(override val values: Value[Ascii]*) extends Response(values*)
    case `Proxy-Authenticate`(override val values: Value[Ascii]*) extends Response(values*)
    case `Public-Key-Pins`(override val values: Value[Ascii]*) extends Response(values*)
    case `Retry-After`(override val values: Value[Ascii]*) extends Response(values*)
    case `Server`(override val values: Value[Ascii]*) extends Response(values*)
    case `Set-Cookie`(override val values: Value[Ascii]*) extends Response(values*)
    case `Strict-Transport-Security`(override val values: Value[Ascii]*) extends Response(values*)
    case `Tk`(override val values: Value[Ascii]*) extends Response(values*)
    case `Vary`(override val values: Value[Ascii]*) extends Response(values*)
    case `WWW-Authenticate`(override val values: Value[Ascii]*) extends Response(values*)
    case `X-Frame-Options`(override val values: Value[Ascii]*) extends Response(values*)
    override val flatten: Ascii = flattenAll(productPrefix, values)
  }

  enum Mutual(val values: Value[Ascii]*) extends Field.Lexeme[Ascii]{
    case `Cache-Control`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Connection`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Content-Encoding`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Content-Length`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Content-MD5`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Content-Type`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Date`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Pragma`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Trailer`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Transfer-Encoding`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Upgrade`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Via`(override val values: Value[Ascii]*) extends Mutual(values*)
    case `Warning`(override val values: Value[Ascii]*) extends Mutual(values*)
    override val flatten: Ascii = flattenAll(productPrefix, values)
  }

}
object Field extends Lexer[Ascii]{

  override type CC[SRC <: Ascii] = Field[SRC]

  override def decode[T <: Ascii](rawData: T): Try[Field[T]] = Try{ new Field[T]{
    override val flatten: T = ???
    override val values: Seq[Value[T]] = ???
  }}

}



