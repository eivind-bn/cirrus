package cirrus.event

import cirrus.event.{Broadcaster, Reporter}
import org.scalatest.wordspec.AnyWordSpec


class BroadcasterTest extends AnyWordSpec {

  val foo = "Foo"
  val bar = "Bar"
  val baz = "Baz"

  "A broadcaster" should {

    "always optionally return a processed argument" when {
      "non-exhausted" in {

        val broadcaster = Broadcaster[String]

        assertResult(Some(foo)){
          broadcaster.dispatch(foo)
        }

        assertResult(Some("FOO")){
          broadcaster.map(_.toUpperCase).dispatch(foo)
        }
        assertResult(Some("foo")){
          broadcaster.collect{ case s => s.toLowerCase }.dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.tapEach(identity).dispatch(foo)
        }
        assertResult(Some("Foo Bar")){
          broadcaster.scanLeft(foo)((x,y) => s"$x $y").dispatch(bar)
        }
        assertResult(List((foo,0),(bar,1),(baz,2))){
          List(foo, bar, baz).flatMap(broadcaster.zipWithIndex.dispatch)
        }
        assertResult(0){
          var result = 0
          val otherBroadcaster = Broadcaster[Int]

          broadcaster.flatMap(s => otherBroadcaster.map(_*s.toInt)).tapEach(int => result += int)
          for(i <- 1 to 3) otherBroadcaster.dispatch(i)
          result
        }
        assertResult(30){
          var result = 0
          val otherBroadcaster = Broadcaster[Int]

          broadcaster.flatMap(s => otherBroadcaster.map(_*s.toInt)).tapEach(int => result += int).dispatch("5")
          for(i <- 1 to 3) otherBroadcaster.dispatch(i)
          result
        }
        assertResult(0){
          var result = 0
          val otherBroadcaster = Broadcaster[Int]

          broadcaster.map(s => otherBroadcaster.map(_*s.toInt)).flatten.tapEach(int => result += int)
          for(i <- 1 to 3) otherBroadcaster.dispatch(i)
          result
        }
        assertResult(30){
          var result = 0
          val otherBroadcaster = Broadcaster[Int]

          broadcaster.map(s => otherBroadcaster.map(_*s.toInt)).flatten.tapEach(int => result += int).dispatch("5")
          for(i <- 1 to 3) otherBroadcaster.dispatch(i)
          result
        }
        assertResult(Some(foo)){
          broadcaster.take(5).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.slice(0,1).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.takeWhile(_ => true).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.dropWhile(_ => false).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.filter(_ => true).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.filterNot(_ => false).dispatch(foo)
        }
        assertResult(Some(foo)){
          broadcaster.drop(0).dispatch(foo)
        }



      }
      "exhausted" in {
        val broadcaster = Broadcaster[String]

        assertResult(None){
          broadcaster.filter(_ => false).dispatch(foo)
        }
        assertResult(List(foo,baz)){
          List(foo,bar,baz).flatMap(broadcaster.filterNot(_ == bar).dispatch)
        }
        assertResult(List(bar)){
          List(foo,bar,baz).flatMap(broadcaster.filter(_ == bar).dispatch)
        }
        assertResult(List(foo)){
          List(foo,bar,baz).flatMap(broadcaster.take(1).dispatch)
        }
        assertResult(List(bar,baz)){
          List(foo,bar,baz).flatMap(broadcaster.drop(1).dispatch)
        }
        assertResult(List(foo)){
          List(foo,bar,baz).flatMap(broadcaster.takeWhile(_.contains("o")).dispatch)
        }
        assertResult(List(bar,baz)){
          List(foo,bar,baz).flatMap(broadcaster.dropWhile(_.contains("o")).dispatch)
        }
        assertResult(List("BAZ")){
          List(foo,bar,baz).flatMap(broadcaster.collect{ case s if s == baz => s.toUpperCase }.dispatch)
        }
        assertResult((0 until 30) -> (0 to 50)){
          val (a,b) = Broadcaster[Int].splitAt(30)
          (0 to 50).flatMap(a.dispatch) -> (0 to 50).flatMap(b.dispatch)
        }
        assertResult((0 until 20) -> (0 to 50)){
          val (a,b) = Broadcaster[Int].span(int => int < 20 || int > 30)
          (0 to 50).flatMap(a.dispatch) -> (0 to 50).flatMap(b.dispatch)
        }
        assertResult(List(foo) -> List(foo,baz)){
          val otherBroadcaster = Reporter[String].filter(s => s == foo || s == baz)
          List(foo,bar,baz).flatMap(broadcaster.filter(_ == foo).prepended(otherBroadcaster).dispatch) ->
            List(foo,bar,baz).flatMap(otherBroadcaster.dispatch)
        }
        assertResult(List(foo) -> List(foo,baz)){
          val otherBroadcaster = Reporter[String].filter(s => s == foo || s == baz)
          List(foo,bar,baz).flatMap(broadcaster.filter(_ == foo).appended(otherBroadcaster).dispatch) ->
            List(foo,bar,baz).flatMap(otherBroadcaster.dispatch)
        }
      }

      "Share traits from both delegate and report. Events should propagate on the entire event graph" in {
        val broadcast = Broadcaster[String]
        var counter = 0

        broadcast.tapEach(_ => counter += 1)
        broadcast.foreach(_ => counter += 1)
        broadcast.map(_ => counter += 1).dispatch("Foo")

        assert(counter == 3)

        broadcast
          .tapEach(_ => counter += 1)
          .map(_.toUpperCase)
          .collect{ case _ => counter += 1; 42 }
          .dispatch("Foo")

        assert(counter == 8)

        val (a,b) = broadcast.splitAt(4)

        a.tapEach(_ => counter += 1)

        (1 to 10).foreach{_ =>
          a.dispatch("Foo")
        }
        assert(counter == 62)

        (1 to 10).foreach{_ =>
          b.tapEach(_ => counter += 1).dispatch("Bar")
        }
        assert(counter == 167)

      }
    }

    "methods behaviour meets expectations" in {
      val root = Broadcaster[Int]

      var counter = 0
      var expected = 0

      extension (event: Broadcaster[Int,_])
        def run(iter: Int, exp: Int): Unit = {
          expected += exp
          (0 until iter).flatMap(event.dispatch).foreach{ _ =>
            counter += 1
          }
        }
        def fail(iter: Int): Unit = (0 until iter).flatMap(event.dispatch).foreach{ _ =>
          org.scalatest.Assertions.fail(s"Expected count '$expected' and actual '$counter' mismatch")
        }


      expected += 500
      root.take(500).foreach(_ => counter += 1)

      root.filter(_ => false).fail(20)
      root.filterNot(_ => true).fail(20)
      root.take(0).fail(20)
      root.takeWhile(_ < 0).fail(20)
      root.drop(10).fail(5)
      root.dropWhile(_ < 50).fail(10)

      root.run(50,50)
      root.filter(_ => true).run(50,50)
      root.filterNot(_ => false).run(50,50)
      root.map(identity).run(50,50)
      root.take(50).run(70,50)
      root.drop(70).run(100,30)
      root.slice(50,500).run(1000,450)
      root.dropWhile(_ < 50).run(300,250)
      root.takeWhile(_ < 100).run(300,100)
      root.scanLeft(0)((x,y) => x*y).run(200,200)
      root.collect(PartialFunction.fromFunction(identity)).run(200,200)
      root.collect(PartialFunction.empty).run(500,0)
      val (a,b) = root.splitAt(100)
      a.run(500,100)
      b.run(500,500)
      val (c,d) = root.span(i => i < 50 || i > 100)
      c.run(500,50)
      d.run(500,500)

      val otherBroadcaster = Broadcaster[Int]
      var block = false
      root.flatMap(_ => otherBroadcaster).filterNot(_ => block).tapEach(i => counter += i).dispatch(0)
      for(i <- 1 to 250) otherBroadcaster.dispatch(i)
      def increasingNaturalsSum(n: Int): Int = (n*(n+1))/2
      expected += increasingNaturalsSum(250)

      root.map(_ => otherBroadcaster).flatten.drop(500).tapEach(i => counter += i).dispatch(0)
      block = true
      for(i <- 1 to 750) otherBroadcaster.dispatch(i)
      expected += increasingNaturalsSum(750) - increasingNaturalsSum(500)


      val newRoot = Broadcaster[Int]
      newRoot.prepended(root).run(500,500)
      root.appended(newRoot)
      newRoot.run(500,500)

      assert(counter == expected)
    }
  }
}
