package protocol

import io.{InputStream, OutputStream}

import java.net.{InetAddress, InetSocketAddress, SocketAddress}
import java.nio.ByteBuffer
import java.nio.channels.{ServerSocketChannel, SocketChannel}
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.Try

trait TCP{



}
object TCP {


  protected object Pool{
    trait Endpoint{
      def socket(): SocketChannel
      def close(): Unit
      def read(byteBuffer: ByteBuffer): Int = socket().read(byteBuffer)
      def write(byteBuffer: ByteBuffer): Int = socket().write(byteBuffer)
    }

    val serverSockets: mutable.HashMap[Int,ServerSocketChannel] = mutable.HashMap.empty
    val socket: mutable.HashMap[(Int,Thread),SocketChannel] = mutable.HashMap.empty

    private def getServerSocket(port: Int): ServerSocketChannel = serverSockets
      .getOrElseUpdate(port, ServerSocketChannel.open().bind(new InetSocketAddress(port)))

    def get(port: Int): Endpoint = new Endpoint{
      override def socket(): SocketChannel = Pool.socket
        .getOrElseUpdate((port,Thread.currentThread()), getServerSocket(port).accept())

      override def close(): Unit = Pool.socket.remove((port, Thread.currentThread())).foreach(_.close())
    }

    Runtime.getRuntime.addShutdownHook(new Thread(() => serverSockets.values.foreach(_.close())))
  }



  def server(port: Int): Try[(InputStream[Byte], OutputStream[Byte])] = Try{
    val in = ByteBuffer.allocate(1)
    val out = ByteBuffer.allocate(1)
    var l = 0

    val inputStream = new InputStream[Byte] {
      override def close(): Unit = Pool.get(port).close()

      override def hasNext: Boolean = if l > 0 then true else {
        l = Pool.get(port).read(in.clear())
        if l > 0 then true else false
      }

      override def next(): Byte = { l -= 1; in.get(0) }
    }

    val outputStream = new OutputStream[Byte] {

      override def fireEvent(data: Byte): Option[Byte] = {
        Pool.get(port).write(out.clear().put(0,data))
        Some(data)
      }

      override def close(): Unit = Pool.get(port).close()
    }

    inputStream -> outputStream
  }


  def client(): (InputStream[Byte], OutputStream[Byte]) = {
    ???
  }



}
