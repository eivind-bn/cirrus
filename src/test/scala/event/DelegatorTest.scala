package event

import event.{Delegate, Delegator, Event, Report, Reporter}
import org.scalatest.wordspec.AnyWordSpec

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

class DelegatorTest extends AnyWordSpec {

  "A delegator" should {
    "always return it's applied argument" when {
      "unconstrained" in {
        val delegator: Delegator[String] = Delegate[String]
        assert(delegator.fireEvent("Hello").contains("Hello"))
      }
      "constrained" in {
        val delegator: Delegator[String] = Delegate[String]
        assert(delegator.filter(_ => false).fireEvent("Hello").contains("Hello"))
      }
    }

    "not relay upwards" in {
      val delegator: Delegator[String] = Delegate[String]
      delegator
        .tapEach(_.ensuring(false))
        .map(_.ensuring(false))
        .collect(_.ensuring(false))
        .fireEvent("Test")

      assertThrows[AssertionError](delegator.fireEvent("Test"))
    }

    "relay downwards" in {
      val root: Delegator[Int] = Delegate[Int]
      val child = root.map(_.ensuring(false))

      val counter = new Array[Int](5)

      child.map(identity).foreach(counter(0) += _)
      child.collect{ case i if i > 3 => i}.foreach(counter(1) += _)
      child.take(2).foreach(counter(2) += _)
      child.drop(2).foreach(counter(3) += _)
      child.slice(4,5).foreach(counter(4) += _)

      assertThrows[AssertionError](root.fireEvent(5))

      (1 to 5).foreach(i => child.fireEvent(i))
      assert(counter sameElements List(15,9,3,12,5))
    }

    "adhere to expected monadic behaviour" in {
      val root: Delegator[Int] = Delegate[Int]

      var counter = 0
      var expected = 0
      val iter = 50

      extension (event: Delegator[_])
        def increment(max: Int = iter): Unit = {
          expected += max
          event.foreach(_ => counter += 1)
        }
        def fail(): Unit = event.foreach{ _ =>
          org.scalatest.Assertions.fail(s"Expected count '$expected' and actual '$counter' mismatch")
        }


      root.filter(_ => false).fail()
      root.filterNot(_ => true).fail()
      root.take(0).fail()
      root.takeWhile(_ < 0).fail()
      root.drop(iter).fail()
      root.dropWhile(_ > iter)

      root.increment()
      root.filter(_ => true).increment()
      root.filterNot(_ => false).increment()
      root.map(identity).increment()
      root.take(3).increment(3)
      root.drop(5).increment(iter - 5)
      root.slice(5,500).increment(iter - 5)
      root.dropWhile(_ < 5).increment(iter - 5)
      root.takeWhile(_ < 10).increment(10)
      root.scanLeft(0)((x,y) => x*y).increment()
      root.collect(PartialFunction.fromFunction(identity)).increment()
      root.collect(PartialFunction.empty).increment(0)
      val (a,b) = root.splitAt(10)
      a.increment(10)
      b.increment(iter - 10)
      val (c,d) = root.span(i => i < 10 && i < 5)
      c.increment(5)
      d.increment(iter - 5)
      root.flatMap(_ => root).increment(((iter*iter)/2) - iter/2)
//      root.map(_ => root).flatten.increment(((iter*iter)/2) - iter/2)

      val newRoot = Delegate[Int]
      newRoot.prepended(root).increment(iter)
      root.appended(newRoot)
      newRoot.increment(iter)

      (0 until iter).foreach(root.fireEvent)
      assert(counter == expected)
    }

    "qualify for garbage collection once exhausted" in {

      // Created anonymous instance
      val delegator: Delegator[Int] = new Delegator[Int]

      delegator.take(1)
      assert(delegator.workers.length == 1)

      delegator.fireEvent(5)
      assert(delegator.workers.isEmpty)


      var counter = 0

      delegator.take(3).foreach(_ => counter += 1)
      delegator.takeWhile(_ < 5).foreach(_ => counter += 1)
      delegator.slice(3,5).foreach(_ => counter += 1)

      assert(delegator.workers.length == 3)

      // trigger take() & takeWhile()
      delegator.fireEvent(2)
      assert(counter == 2)

      // trigger take() & exhaust takeWhile()
      delegator.fireEvent(6)
      assert(counter == 3)
      assert(delegator.workers.length == 2)

      // trigger and exhaust take()
      delegator.fireEvent(8)
      assert(counter == 4)
      assert(delegator.workers.length == 1)

      // trigger slice() and exhaust it.
      delegator.fireEvent(9)
      delegator.fireEvent(9)
      assert(counter == 6)
      assert(delegator.workers.isEmpty)

      delegator.fireEvent(9)
      assert(counter == 6)
      assert(delegator.workers.isEmpty)

    }
  }
}
