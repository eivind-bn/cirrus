package event


import automation.FiniteStateMachine
import combinator.PureStream


abstract class Broadcaster[I,O] extends Event[I,O,Broadcaster] with PureStream[I,O,Broadcaster] { parent =>


  protected val delegator: Delegator[O] = Delegate[O]
  protected val reporter: Reporter[I, O]


  trait Relay[I1, O1] extends Broadcaster[I, O1] {
    var last: Option[O1] = None
    def getAndReset[T]: PartialFunction[T, O1] = { case _ if last.isDefined => val temp = last.get; last = None; temp }
    override val reporter: Reporter[I, O1] = parent.reporter.collect(getAndReset)
    override val delegator: Delegator[O1] = parent.delegator.collect(transformer).tapEach(o1 => last = Some(o1))
    override def fireEvent(data: I): Option[O1] = data =>: reporter
    def transformer: PartialFunction[O, O1]
  }


  override def prepended[B, DD[_, O] <: Event[_, O, DD]](other: Event[B, I, DD]): Broadcaster[B, O] = new Broadcaster[B, O] {
    override protected val reporter: Reporter[B, O] = (data: B) => other.fireEvent(data).flatMap { i => parent.fireEvent(i) }
    override def fireEvent(data: B): Option[O] = reporter.fireEvent(data)
  }


  override def appended[B, DD[_, O] <: Event[_, O, DD]](other: Event[O, B, DD]): Broadcaster[I, B] = new Broadcaster[I, B] {
    override protected val reporter: Reporter[I, B] = (data: I) => parent.fireEvent(data).flatMap { o => other.fireEvent(o) }
    override def fireEvent(data: I): Option[B] = reporter.fireEvent(data)
  }


  override def scanLeft[B](z: B)(op: (B, O) => B): Broadcaster[I, B] = new Relay[I, B] {
    override def transformer: PartialFunction[O, B] = o => op(z, o)
  }


  override def filter(pred: O => Boolean): Broadcaster[I, O] = new Relay[I, O] {
    override def transformer: PartialFunction[O, O] = {
      case o if pred(o) => o
    }
  }


  override def filterNot(pred: O => Boolean): Broadcaster[I, O] = filter(pred.andThen(!_))


  override def take(n: Int): Broadcaster[I, O] = new Relay[I, O] {
    var i: Int = 0
    override def transformer: PartialFunction[O, O] = {
      case o if i < n => i += 1; o
    }
  }


  override def takeWhile(p: O => Boolean): Broadcaster[I, O] = new Relay[I, O] {
    var flag = true
    override def transformer: PartialFunction[O, O] = new PartialFunction[O, O] {
      override def isDefinedAt(x: O): Boolean = flag && {
        flag = p(x)
        flag
      }
      override def apply(v1: O): O = v1
    }
  }


  override def drop(n: Int): Broadcaster[I, O] = new Relay[I, O] {
    var i: Int = 0
    override def transformer: PartialFunction[O, O] = {
      case o if i >= n => o
      case _ if {
        i += 1; false
      } => throw new Exception
    }
  }


  override def dropWhile(p: O => Boolean): Broadcaster[I, O] = new Relay[I, O] {
    var flag = false
    override def transformer: PartialFunction[O, O] = new PartialFunction[O, O] {
      override def isDefinedAt(x: O): Boolean = flag || {
        flag = !p(x)
        flag
      }
      override def apply(v1: O): O = v1
    }
  }


  override def slice(from: Int, until: Int): Broadcaster[I, O] = new Relay[I, O] {
    var i: Int = 0
    override def transformer: PartialFunction[O, O] = new PartialFunction[O, O] {
      override def isDefinedAt(x: O): Boolean = i match {
        case n if n < until && n >= from => true
        case n if n < until => i += 1; false
        case _ => false
      }
      override def apply(v1: O): O = {
        i += 1; v1
      }
    }
  }


  override def map[B](f: O => B): Broadcaster[I, B] = new Relay[I, B] {
    override def transformer: PartialFunction[O, B] = o => f(o)
  }


  override def flatMap[B, DD[_, O] <: PureStream[_, O, DD]](f: O => PureStream[I, B, DD]): Broadcaster[I, B] = new Relay[I, B] {
    override val reporter: Reporter[I, B] = (data: I) => parent.reporter.fireEvent(data)
      .map { o => f(o) }
      .flatMap {
        case event: Event[I, B, DD] => event.fireEvent(data)
        case stream => stream.spinWait()
      }
      .tapEach { b => last = Some(b) }
      .lastOption
    override val delegator: Delegator[B] = parent.delegator.collect { case _ if last.isDefined => last.get }

    override def transformer: PartialFunction[O, B] = PartialFunction.empty
  }


  override def collect[B](pf: PartialFunction[O, B]): Broadcaster[I, B] = new Relay[I, B] {
    override def transformer: PartialFunction[O, B] = pf
  }


  override def zipWithIndex: Broadcaster[I, (O, Int)] = new Relay[I, (O, Int)] {
    var i: Int = -1
    override def transformer: PartialFunction[O, (O, Int)] = new PartialFunction[O, (O, Int)] {
      override def isDefinedAt(x: O): Boolean = true
      override def apply(v1: O): (O, Int) = {
        i += 1; v1 -> i
      }
    }
  }


  override def span(p: O => Boolean): (Broadcaster[I, O], Broadcaster[I, O]) = {

    var flag = true

    val (leftDelegate, rightDelegate) = parent.delegator.span(o => flag && { flag = p(o); flag })

    val (leftReporter, rightReporter) = parent.reporter.span(o => flag && { flag = p(o); flag })

    val left = new Relay[I, O] {
      override def transformer: PartialFunction[O, O] = identity(_)

      override val delegator: Delegator[O] = leftDelegate.tapEach { o => last = Some(o) }
      override val reporter: Reporter[I, O] = leftReporter.collect(getAndReset)
    }

    val right = new Relay[I, O] {
      override def transformer: PartialFunction[O, O] = identity(_)

      override val delegator: Delegator[O] = rightDelegate.tapEach { o => last = Some(o) }
      override val reporter: Reporter[I, O] = rightReporter.collect(getAndReset)
    }

    left -> right
  }


  override def splitAt(n: Int): (Broadcaster[I, O], Broadcaster[I, O]) = {

    val (leftDelegate, rightDelegate) = parent.delegator.splitAt(n)
    val (leftReporter, rightReporter) = parent.reporter.splitAt(n)

    val left = new Relay[I, O] {
      override def transformer: PartialFunction[O, O] = identity(_)

      override val delegator: Delegator[O] = leftDelegate.tapEach { o => last = Some(o) }
      override val reporter: Reporter[I, O] = leftReporter.collect(getAndReset)
    }

    val right = new Relay[I, O] {
      override def transformer: PartialFunction[O, O] = identity(_)

      override val delegator: Delegator[O] = rightDelegate.tapEach { o => last = Some(o) }
      override val reporter: Reporter[I, O] = rightReporter.collect(getAndReset)
    }

    left -> right
  }


  override def tapEach[U](f: O => U): Broadcaster[I, O] = new Relay[I, O] {
    override def transformer: PartialFunction[O, O] = o => {
      f(o); o
    }
  }


  override def foreach(f: O => Unit): Unit = new Relay[I, Unit] {
    override def transformer: PartialFunction[O, Unit] = f(_)
  }


  override def spinWait(waitFunc: => Unit = Thread.onSpinWait()): Option[O] = {
    var capture: Option[O] = None
    delegator.take(1).foreach { o => capture = Some(o) }
    while (capture.isEmpty) waitFunc
    capture
  }
  

}

object Broadcaster{

  def apply[O]: Broadcaster[O,O] = new Broadcaster[O,O] {
    override val reporter: Reporter[O,O] = Report[O].tapEach{ o => delegator.fireEvent(o) }
    override def fireEvent(data: O): Option[O] = data =>: delegator
  }

}
