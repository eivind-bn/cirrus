package cirrus.automation

import cirrus.combinator.ModularStream
import cirrus.event.{Broadcaster, Delegator, EventTree, Reporter}

trait RollingStateMachine[X,Y] extends StateMachine[X,Y,RollingStateMachine] { parent =>

  type Dispatcher[I,O] <: EventTree[I,O,Dispatcher]
  protected val dispatcher: Dispatcher[X,Y]

  abstract class Relay[Z,S](val initialState: S) extends RollingStateMachine[X,Z]{
    override type Dispatcher[X,Y] = parent.Dispatcher[X,Y]
    protected var state: S = this.initialState
    def reset(): Unit = this.state = this.initialState
    def setState(newState: S): Unit = this.state = newState
  }


  override def dispatch(data: X): Option[Y] = dispatcher.dispatch(data)


  def prepended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[B,X,DD]): RollingStateMachine[B,Y] = new RollingStateMachine[B,Y]{
    override type Dispatcher[X,Y] = parent.Dispatcher[X,Y]
    override protected val dispatcher: this.Dispatcher[B, Y] = parent.dispatcher.prepended(other)
  }

  def appended[B,DD[_,O] <: ModularStream[_,O,DD]](other: ModularStream[Y,B,DD]): RollingStateMachine[X,B] = new RollingStateMachine[X,B]{
    override type Dispatcher[X,Y] = parent.Dispatcher[X,Y]
    override protected val dispatcher: this.Dispatcher[X, B] = parent.dispatcher.appended(other)
  }

  override def map[Z,S](initState: S)(f: (S, Y) ~> (S, Z)): RollingStateMachine[X,Z] = new Relay[Z,S](initState) {
    override protected val dispatcher: this.Dispatcher[X,Z] = parent.dispatcher
      .map{ o => f.unapply(this.state -> o).toLeft(f.unapply(this.initialState -> o)) }
      .map{
        case Left((newState, output)) =>
          setState(newState)
          Some(output)

        case Right(Some((newState, output))) =>
          setState(newState)
          Some(output)

        case Right(None) =>
          reset()
          None
      }
      .collect{ case Some(value) => value }
  }

  override def stackMap[B,S](initState: S)(f: (S, Y) ~> (S, B)): RollingStateMachine[X,Seq[B]] = new Relay[Seq[B],S](initState) {
    var buffer: Seq[B] = Seq.empty
    override protected val dispatcher: parent.Dispatcher[X, Seq[B]] = parent.dispatcher
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


  override def tapEach[U,S](initState: S)(f: (S, Y) ~> (S, U)): RollingStateMachine[X,Y] = new Relay[Y,S](initState) {
    override protected val dispatcher: parent.Dispatcher[X, Y] = parent.dispatcher
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

  override def foreach(f: Y => Unit): Unit = dispatcher.foreach(f)

}
object RollingStateMachine {

  def apply[T,CC[I,O] <: EventTree[I,O,CC]](x: EventTree[T,T,CC]): RollingStateMachine[T,T]{
    type Dispatcher[X,Y] = CC[X,Y]
  } = new RollingStateMachine[T,T]{ self =>
    override type Dispatcher[X,Y] = CC[X,Y]
    override protected val dispatcher: CC[T,T] = x.tapEach(identity)
  }

}
