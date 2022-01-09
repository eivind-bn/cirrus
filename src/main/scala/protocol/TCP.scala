package protocol

import java.net.InetAddress

class TCP(inetAddress: InetAddress, port: Option[Int] = None) {

  def connect: Session = new Session {
    override def hasNext: Boolean = ???

    override def next(): Byte = ???
  }


  trait Session extends Iterator[Byte] {
    override def hasNext: Boolean
    override def next(): Byte
  }

}
object TCP
