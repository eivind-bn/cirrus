package protocol.http

import protocol.lang.{Ascii, US_ASCII}
import protocol.{Lexer, Reader}

import scala.util.Try
import protocol.Reader


sealed trait StatusCode[T <: Ascii] extends StatusCode.Lexeme[T]

object StatusCodes{
  export Info._
  enum Info extends StatusCode[Ascii] {
    case `100 Continue` extends Info
    case `101 Switching Protocols` extends Info
    case `102 Processing` extends Info
    case `103 Early Hints` extends Info
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }
  export Success._
  enum Success extends StatusCode[Ascii] {
    case`200 OK` extends Success
    case`201 Created` extends Success
    case`202 Accepted` extends Success
    case`203 Non-Authoritative Information` extends Success
    case`204 No Content` extends Success
    case`205 Reset Content` extends Success
    case`206 Partial Content` extends Success
    case`207 Multi-Status` extends Success
    case`208 Already Reported` extends Success
    case`226 IM Used` extends Success
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }
  export Redirection._
  enum Redirection extends StatusCode[Ascii]{
    case`300 Multiple Choices` extends Redirection
    case`301 Moved Permanently` extends Redirection
    case`302 Found` extends Redirection
    case`303 See Other` extends Redirection
    case`304 Not Modified` extends Redirection
    case`305 Use Proxy` extends Redirection
    case`306 Switch Proxy` extends Redirection
    case`307 Temporary Redirect` extends Redirection
    case`308 Permanent Redirect` extends Redirection
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }
  export ClientError._
  enum ClientError extends StatusCode[Ascii]{
    case`400 Bad Request` extends ClientError
    case`401 Unauthorized` extends ClientError
    case`402 Payment Required` extends ClientError
    case`403 Forbidden` extends ClientError
    case`404 Not Found` extends ClientError
    case`405 Method Not Allowed` extends ClientError
    case`406 Not Acceptable` extends ClientError
    case`407 Proxy Authentication Required` extends ClientError
    case`408 Request Timeout` extends ClientError
    case`409 Conflict` extends ClientError
    case`410 Gone` extends ClientError
    case`411 Length Required` extends ClientError
    case`412 Precondition Failed` extends ClientError
    case`413 Payload Too Large` extends ClientError
    case`414 URI Too Long` extends ClientError
    case`415 Unsupported Media Type` extends ClientError
    case`416 Range Not Satisfiable` extends ClientError
    case`417 Expectation Failed` extends ClientError
    case`418 I'm a teapot` extends ClientError
    case`421 Misdirected Request` extends ClientError
    case`422 Unprocessable Entity` extends ClientError
    case`423 Locked` extends ClientError
    case`424 Failed Dependency` extends ClientError
    case`425 Too Early` extends ClientError
    case`426 Upgrade Required` extends ClientError
    case`428 Precondition Required` extends ClientError
    case`429 Too Many Requests` extends ClientError
    case`431 Request Header Fields Too Large` extends ClientError
    case`451 Unavailable For Legal Reasons` extends ClientError
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }
  export ServerError._
  enum ServerError extends StatusCode[Ascii]{
    case`500 Internal Server Error` extends ServerError
    case`501 Not Implemented` extends ServerError
    case`502 Bad Gateway` extends ServerError
    case`503 Service Unavailable` extends ServerError
    case`504 Gateway Timeout` extends ServerError
    case`505 HTTP Version Not Supported` extends ServerError
    case`506 Variant Also Negotiates` extends ServerError
    case`507 Insufficient Storage` extends ServerError
    case`508 Loop Detected` extends ServerError
    case`510 Not Extended` extends ServerError
    case`511 Network Authentication Required` extends ServerError
    override val flatten: Ascii = Ascii.decode(productPrefix).get
  }
}
object StatusCode extends Lexer[Ascii]{


  override type CC[T <: Ascii] = StatusCode[T]


  override def decode[T <: Ascii](rawData: T): Try[StatusCode[T]] = Try{ new StatusCode[T] {
    override val flatten: T = rawData
  }}


}

