import event.{Broadcast, Delegate, Report, Reporter}

object Test extends App {

  val a = Broadcast[String]
  val b = Report[String].tapEach(s => println(s -> "3")).tapEach(s => println(s -> "2"))

  val c = a.tapEach(s => println(s -> "0")).flatMap(_ => b).tapEach(s => println(s -> "1"))

  println(a.dispatch("a"))
  println(b.dispatch("b"))
  println(c.dispatch("c"))
  println(b.dispatch("b"))




}
