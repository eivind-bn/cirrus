import protocol.{Codec, Protocol, TCP}
import event._
import io.InputStream
import pattern.Extractor

import java.util.{Timer, TimerTask}


object Demo extends App{



  val delegate = Delegate[String].tapEach(s => println(s"delegate1: ${s}"))
  val report = Report[String].tapEach(s => println(s"report1: ${s}")).map(s => s"report mapped ${s}")
  val broadcast = Broadcast[String].tapEach(s => println(s"broadcast1: ${s}"))

  delegate.tapEach(s => println(s"delegate2: ${s}"))
  report.tapEach(s => println(s"report2: ${s}"))
  broadcast.tapEach(s => println(s"broadcast2: ${s}"))

  "Hello" =>: report ~ delegate ~ broadcast





//  val list = List("A","B","C").iterator
//
//  val l1 = Report[String].collect{ case s if list.hasNext => s"$s + ${list.next()}" }
//  val l2 = Report[String].flatMap(_ => l1).tapEach(println)
//
//  l1.tapEach(println).fireEvent("Hola")
//  l2.fireEvent("Hello")
//  l2.fireEvent("Hello")
//  l2.fireEvent("Hello")
//  l2.fireEvent("Hello")


//  val b = Report[Int].slice(0,11).tapEach(println)
//  (1 to 10).foreach(b.fireEvent)
//
//
//  new Timer(true).schedule(new TimerTask {
//    override def run(): Unit = b.fireEvent(42)
//  }, 3000)
//
//  new Thread(() => println(b.spinWait())).start()




}
