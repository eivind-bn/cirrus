package protocol.tcp

import io.Socket
import pattern.Extractor
import protocol.{Codec, tcp}

import java.net.{InetAddress, InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

trait TCP[T <: Iterable[Byte]] extends TCP.Protocol[T]
object TCP extends Codec[Iterable[Byte]] with Socket[Byte,Byte] {
  override type CC[T <: Iterable[Byte]] = TCP[T]

  override def decode[T <: Iterable[Byte]](rawData: T): Try[TCP[T]] = ???
}

