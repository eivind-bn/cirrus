import protocol.TCP
import event._

object Demo extends App{




  val x = Broadcast[String].tapEach(_ => println("1"))
  val y = Broadcast[String].tapEach(_ => println("2"))

  (x :: y).fireEvent("44")







}
