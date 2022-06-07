package automation

import combinator.{ImpureStream, PureStream}
import event.*

trait RotaryStateMachine[I,O,CC[I,O] <: EventStream[I,O,CC]] extends StateMachine[I,O,CC] { parent =>

  export eventLoop.dispatch
  protected val eventLoop: CC[I,O]

  trait Tail[I1,O1,S] extends RotaryStateMachine[I1,O1,CC]{
    def initialState: S
    protected var state: S = initialState
    def reset(): Unit = this.state = initialState
  }


  def prepended[B, DD[_, O] <: EventStream[_, O, DD]](other: EventStream[B, I, DD]): RotaryStateMachine[B,O,CC] = new RotaryStateMachine[B,O,CC] {
    override protected val eventLoop: CC[B, O] = parent.eventLoop.prepended(other)
  }


  def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[O,B,DD]): RotaryStateMachine[I,B,CC] = new RotaryStateMachine[I,B,CC] {
    override protected val eventLoop: CC[I, B] = parent.eventLoop.appended(other)
  }


  override def map[B,S](initState: S)(f: (S, O) ~> (S, B)): RotaryStateMachine[I,B,CC] = new Tail[I,B,S] {
    override def initialState: S = initState
    override protected val eventLoop: CC[I, B] = parent.eventLoop ~ Report[O]
      .map{ o => f.unapply(this.state -> o).toLeft(f.unapply(this.initialState -> o)) }
      .map{

        case Left((newState, output)) =>
          this.state = newState
          Some(output)

        case Right(Some((newState, output))) =>
          this.state = newState
          Some(output)

        case Right(None) =>
          this.state = this.initialState
          None

      }
      .collect{ case Some(value) => value }
  }

  override def stackMap[B,S](initState: S)(f: (S, O) ~> (S, B)): RotaryStateMachine[I,Seq[B],CC] = new Tail[I,Seq[B],S] {
    var buffer: Seq[B] = Seq.empty
    override def initialState: S = initState
    override protected val eventLoop: CC[I, Seq[B]] = parent.eventLoop ~ Report[O]
      .map{ o => f.unapply(this.state -> o).toLeft(f.unapply(this.initialState -> o)) }
      .map {

        case Left((newState, output)) =>
          this.state = newState
          buffer :+= output
          None

        case Right(Some((newState, output))) =>
          this.state = newState
          val temp = buffer
          buffer = Seq(output)
          Some(temp)

        case Right(None) =>
          reset()
          None

      }
      .collect{ case Some(value) => value }
  }


  override def tapEach[U,S](initState: S)(f: (S, O) ~> (S, U)): RotaryStateMachine[I,O,CC] = new Tail[I,O,S] {
    override def initialState: S = initState
    override protected val eventLoop: CC[I, O] = parent.eventLoop ~ Report[O]
      .map{ o => f.unapply(this.state -> o).toLeft(f.unapply(this.initialState -> o)) match {

          case Left((newState, output)) =>
            this.state = newState
            Some(o)

          case Right(Some((newState, output))) =>
            this.state = newState
            Some(o)

          case Right(value) =>
            reset()
            None

        }}
      .collect{ case Some(value) => value }
  }


  override def foreach(f: O => Unit): Unit = eventLoop.foreach(f)

}
object RotaryStateMachine{

  def broadcast[O]: RotaryStateMachine[O,O,Broadcaster] = new RotaryStateMachine[O,O,Broadcaster] {
    override protected val eventLoop: Broadcaster[O, O] = Broadcast[O]
  }

  def report[O]: RotaryStateMachine[O,O,Reporter] = new RotaryStateMachine[O,O,Reporter] {
    override protected val eventLoop: Reporter[O, O] = Report[O]
  }

  //TODO fix generic constraint issue.
//  def delegate[O]: RotaryStateMachine[O,O,[_,X] =>> Delegator[X]] = new RotaryStateMachine[O,O,[_,X] =>> Delegator[X]] {
//    override protected val eventLoop: Delegator[O] = Delegate[O]
//  }

}
