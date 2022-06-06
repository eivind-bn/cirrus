package event

import org.scalatest.wordspec.AnyWordSpec

class ReporterTest extends AnyWordSpec {


  "A reporter" should {

    "return some processed output when prior reporters are non-exhausted" in {

      val stringReporter = Reporter[String]
      val intReporter = Reporter[Int]

      assertResult(Some("Foo")){
        stringReporter.dispatch("Foo")
      }
      assertResult(0 to 100){
        (0 to 100).flatMap(intReporter.dispatch)
      }
      assertResult(Some("FOO")){
        stringReporter.map(_.toUpperCase).dispatch("Foo")
      }
      assertResult(Some("foo")){
        stringReporter.collect{ case s => s.toLowerCase }.dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.tapEach(identity).dispatch("Foo")
      }
      assertResult(Some("Foo Bar")){
        stringReporter.scanLeft("Foo")((x,y) => s"$x $y").dispatch("Bar")
      }
      assertResult(List(("Foo",0),("Bar",1),("Baz",2))){
        List("Foo", "Bar", "Baz").flatMap(stringReporter.zipWithIndex.dispatch)
      }
      assertResult(0){
        var result = 0
        val x = Delegator[Int]
        stringReporter.flatMap(string => x).tapEach(int => result += int)
        result
      }
      assertResult(6){
        var result = 0
        val x = Delegator[Int]
        stringReporter.flatMap(string => x).tapEach(int => result += int).dispatch("foo")
        for(i <- 1 to 3) x.dispatch(i)
        result
      }
      assertResult(0){
        var result = 0
        val x = Delegator[Int]
        stringReporter.map(string => x).flatten.tapEach(int => result += int)
        result
      }
      assertResult(6){
        var result = 0
        val x = Delegator[Int]
        stringReporter.map(string => x).flatten.tapEach(int => result += int).dispatch("foo")
        for(i <- 1 to 3) x.dispatch(i)
        result
      }
      assertResult(Some("Foo")){
        stringReporter.take(5).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.slice(0,1).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.takeWhile(_ => true).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.dropWhile(_ => false).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.filter(_ => true).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.filterNot(_ => false).dispatch("Foo")
      }
      assertResult(Some("Foo")){
        stringReporter.drop(0).dispatch("Foo")
      }
    }

    "return none when exhausted by some composed reporter" in {

      val reporter = Reporter[String]

      assertResult(List("Foo","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.filterNot(_ == "Bar").dispatch)
      }
      assertResult(List("Bar")){
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Bar").dispatch)
      }
      assertResult(List("Foo")){
        List("Foo","Bar","Baz").flatMap(reporter.take(1).dispatch)
      }
      assertResult(List("Bar","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.drop(1).dispatch)
      }
      assertResult(List("Foo")){
        List("Foo","Bar","Baz").flatMap(reporter.takeWhile(_.contains("o")).dispatch)
      }
      assertResult(List("Bar","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.dropWhile(_.contains("o")).dispatch)
      }
      assertResult(List("BAZ")){
        List("Foo","Bar","Baz").flatMap(reporter.collect{ case s if s == "Baz" => s.toUpperCase }.dispatch)
      }
      assertResult(List("Foo") -> List("Foo","Bar","Baz")){
        val (a,b) = reporter.splitAt(1)
        List("Foo","Bar","Baz").flatMap(a.dispatch) -> List("Foo","Bar","Baz").flatMap(b.dispatch)
      }
      assertResult(List("Foo") -> List("Foo","Bar","Baz")){
        val (a,b) = reporter.span(!_.contains("a"))
        List("Foo","Bar","Baz").flatMap(a.dispatch) -> List("Foo","Bar","Baz").flatMap(b.dispatch)
      }
      assertResult((0 until 30) -> (0 to 50)){
        val (a,b) = Reporter[Int].splitAt(30)
        (0 to 50).flatMap(a.dispatch) -> (0 to 50).flatMap(b.dispatch)
      }
      assertResult((0 until 20) -> (0 to 50)){
        val (a,b) = Reporter[Int].span(int => int < 20 || int > 30)
        (0 to 50).flatMap(a.dispatch) -> (0 to 50).flatMap(b.dispatch)
      }
      assertResult(List("Foo") -> List("Foo","Baz")){
        val otherReporter = Reporter[String].filter(s => s == "Foo" || s == "Baz")
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Foo").prepended(otherReporter).dispatch) ->
          List("Foo","Bar","Baz").flatMap(otherReporter.dispatch)
      }
      assertResult(List("Foo") -> List("Foo","Baz")){
        val otherReporter = Reporter[String].filter(s => s == "Foo" || s == "Baz")
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Foo").appended(otherReporter).dispatch) ->
          List("Foo","Bar","Baz").flatMap(otherReporter.dispatch)
      }
    }

    "Be fully immutable. Not relay events to other branches." in {
      val reporter = Reporter[String]

      //Other branches not affected unless fired upon.
      reporter.tapEach(_ => fail())
      reporter.foreach(_ => fail())
      reporter.map(_ => fail())

      // Events travel here only
      var counter = 0
      reporter
        .tapEach(_ => counter += 1)
        .map(_.toUpperCase)
        .collect{ case _ => counter += 1; 42 }
        .dispatch("Foo")

      assert(counter == 2)

      val (a,b) = reporter
        .zipWithIndex
        .span{ case (str, i) => i < 4 }

      (1 to 10).foreach{_ =>
        a.tapEach(_ => counter += 1).dispatch("Foo")
      }
      assert(counter == 6)

      (1 to 10).foreach{_ =>
        b.tapEach(_ => counter += 1).dispatch("Bar")
      }
      assert(counter == 16)
    }

    "methods behaviour meets expectations" in {
      val root = Reporter[Int]

      var counter = 0
      var expected = 0

      extension (event: Reporter[Int,_])
        def run(iter: Int, exp: Int): Unit = {
          expected += exp
          (0 until iter).flatMap(event.dispatch).foreach{ _ =>
            counter += 1
          }
        }
        def fail(iter: Int): Unit = (0 until iter).flatMap(event.dispatch).foreach{ _ =>
          org.scalatest.Assertions.fail(s"Expected count '$expected' and actual '$counter' mismatch")
        }


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
      root.flatMap(_ => root).run(500,500)
      root.map(_ => root).flatten.run(500,500)

      val newRoot = Reporter[Int]
      newRoot.prepended(root).run(500,500)
      root.appended(newRoot)
      newRoot.run(500,500)

      assert(counter == expected)
    }
  }

}
