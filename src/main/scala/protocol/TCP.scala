package protocol

import java.net.{InetAddress, InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

trait TCP[S] extends Protocol[S]
object TCP extends Codec[Byte,TCP] {


}
