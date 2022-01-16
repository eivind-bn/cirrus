package event

import org.scalatest.wordspec.AnyWordSpec

class ReporterTest  extends AnyWordSpec {


  "A reporter" should {

    "return some processed output when prior reporters are non-exhausted" in {

      val reporter = Report[String]


      assertResult(Some("Foo")){
        reporter.fireEvent("Foo")
      }
      assertResult(Some("FOO")){
        reporter.map(_.toUpperCase).fireEvent("Foo")
      }
      assertResult(Some("foo")){
        reporter.collect{ case s => s.toLowerCase }.fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.tapEach(identity).fireEvent("Foo")
      }
      assertResult(Some("Foo Bar")){
        reporter.scanLeft("Foo")((x,y) => s"$x $y").fireEvent("Bar")
      }
      assertResult(List(("Foo",0),("Bar",1),("Baz",2))){
        List("Foo", "Bar", "Baz").flatMap(reporter.zipWithIndex.fireEvent)
      }
      assertResult(Some(42)){
        reporter.flatMap(_ => Report[String].map(_.toInt)).fireEvent("42")
      }
      assertResult(Some(42)){
        reporter.map(_ => Report[String].map(_.toInt)).flatten.fireEvent("42")
      }
      assertResult(Some("Foo")){
        reporter.take(5).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.slice(0,1).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.takeWhile(_ => true).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.dropWhile(_ => false).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.filter(_ => true).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.filterNot(_ => false).fireEvent("Foo")
      }
      assertResult(Some("Foo")){
        reporter.drop(0).fireEvent("Foo")
      }
    }

    "return none when exhausted by some composed reporter" in {

      val reporter = Report[String]

      assertResult(List("Foo","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.filterNot(_ == "Bar").fireEvent)
      }
      assertResult(List("Bar")){
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Bar").fireEvent)
      }
      assertResult(List("Foo")){
        List("Foo","Bar","Baz").flatMap(reporter.take(1).fireEvent)
      }
      assertResult(List("Bar","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.drop(1).fireEvent)
      }
      assertResult(List("Foo")){
        List("Foo","Bar","Baz").flatMap(reporter.takeWhile(_.contains("o")).fireEvent)
      }
      assertResult(List("Bar","Baz")){
        List("Foo","Bar","Baz").flatMap(reporter.dropWhile(_.contains("o")).fireEvent)
      }
      assertResult(List("BAZ")){
        List("Foo","Bar","Baz").flatMap(reporter.collect{ case s if s == "Baz" => s.toUpperCase }.fireEvent)
      }
      assertResult(List("Foo") -> List("Foo","Bar","Baz")){
        val (a,b) = reporter.splitAt(1)
        List("Foo","Bar","Baz").flatMap(a.fireEvent) -> List("Foo","Bar","Baz").flatMap(b.fireEvent)
      }
      assertResult(List("Foo") -> List("Foo","Bar","Baz")){
        val (a,b) = reporter.span(!_.contains("a"))
        List("Foo","Bar","Baz").flatMap(a.fireEvent) -> List("Foo","Bar","Baz").flatMap(b.fireEvent)
      }
      assertResult(List("Foo") -> List("Foo","Baz")){
        val otherReporter = Report[String].filter(s => s == "Foo" || s == "Baz")
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Foo").prepended(otherReporter).fireEvent) ->
          List("Foo","Bar","Baz").flatMap(otherReporter.fireEvent)
      }
      assertResult(List("Foo") -> List("Foo","Baz")){
        val otherReporter = Report[String].filter(s => s == "Foo" || s == "Baz")
        List("Foo","Bar","Baz").flatMap(reporter.filter(_ == "Foo").appended(otherReporter).fireEvent) ->
          List("Foo","Bar","Baz").flatMap(otherReporter.fireEvent)
      }
    }

    "Be fully immutable. Not relay events to other branches." in {
      val reporter = Report[String]

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
        .fireEvent("Foo")

      assert(counter == 2)

      val (a,b) = reporter
        .zipWithIndex
        .span{ case (str, i) => i < 4 }

      (1 to 10).foreach{_ =>
        a.tapEach(_ => counter += 1).fireEvent("Foo")
      }
      assert(counter == 6)

      (1 to 10).foreach{_ =>
        b.tapEach(_ => counter += 1).fireEvent("Bar")
      }
      assert(counter == 16)
    }

    "adhere to expected monadic behaviour" in {
      val root = Report[Int]

      var counter = 0
      var expected = 0

      extension (event: Reporter[Int,_])
        def run(iter: Int, exp: Int): Unit = {
          expected += exp
          (0 until iter).flatMap(event.fireEvent).foreach{_ =>
            counter += 1
          }
        }
        def fail(iter: Int): Unit = (0 until iter).flatMap(event.fireEvent).foreach{ _ =>
          org.scalatest.Assertions.fail(s"Expected count '$expected' and actual '$counter' mismatch")
        }


      root.filter(_ => false).fail(20)
      root.filterNot(_ => true).fail(20)
      root.take(0).fail(20)
      root.takeWhile(_ < 0).fail(20)
      root.drop(10).fail(5)
      root.dropWhile(_ > 0).fail(10)

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

      val newRoot = Report[Int]
      newRoot.prepended(root).run(500,500)
      root.appended(newRoot)
      newRoot.run(500,500)

      assert(counter == expected)
    }
  }

}