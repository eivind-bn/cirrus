


import scala.concurrent.Future
import concurrent.ExecutionContext.Implicits.global

import event._

object Demo extends App{



//  val root = Delegate[String]
//  val (x,y) = root.splitAt(5)
//  x.zipWithIndex.collect{
//    case (str, 3) => println("Number 3")
//    case (str, i) => println(i)
//  }
//  y.zipWithIndex.drop(2).tapEach(s => println(s))
//
//  for(i <- 1 to 20){
//    root.fireEvent("Hello")
//  }


  //  val (a,b) = Branch[String]
//    .span(!_.equals("Hello a7"))
//
//  val c = Branch[String].zipWithIndex.tapEach(println)
//
//  for(i <- 1 to 10)
//    a.tapEach(println).fireEvent(s"Hello a$i")
//    b.tapEach(println).fireEvent(s"Hello b$i")

//  val b = Broadcast[String].tapEach(println).tapEach(println)
//
//  b.tapEach(println).map(_ => 2).tapEach(println).fireEvent("Hello")


  val b = Broadcast[String].tapEach(println)

  val c = b.tapEach(println).filterNot(_.equals("Hallo")).tapEach(println).map(_ => 2).tapEach(println)

  b.fireEvent("Hello")
  b.fireEvent("Hallo")

  val x = Broadcast[String]







}
