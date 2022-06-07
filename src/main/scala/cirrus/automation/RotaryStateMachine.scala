package cirrus.automation

import cirrus.automation.RotaryStateMachine.Dispatcher
import cirrus.combinator.EventStream
import cirrus.event.{Broadcaster, EventTree, Reporter}

trait RotaryStateMachine[X,Y] extends StateMachine[X,Y,RotaryStateMachine] { parent =>

  protected val dispatcher: EventTree.Dispatcher[X,Y]

  trait Relay[Z,S] extends RotaryStateMachine[X,Z]{


    override protected val dispatcher: EventTree.Dispatcher[X, Z] = ???

    def initialState: S
    protected var state: S = initialState
    def reset(): Unit = this.state = initialState
  }


  override def dispatch(data: X): Option[Y] = ???

  def prepended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[B,X,DD]): RotaryStateMachine[B,Y]


  def appended[B,DD[_,O] <: EventStream[_,O,DD]](other: EventStream[Y,B,DD]): RotaryStateMachine[X,B]

  override def map[B,S](initState: S)(f: (S, Y) ~> (S, B)): RotaryStateMachine[X,B] = new Relay[X,B,S] {
    override def initialState: S = initState
    override protected val dispatcher: parent.Dispatcher[X, B] = parent.dispatcher ~ Reporter[Y]
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

  override def stackMap[B,S](initState: S)(f: (S, Y) ~> (S, B)): RotaryStateMachine[X,Seq[B]] = new Relay[X,Seq[B],S] {
    var buffer: Seq[B] = Seq.empty
    override def initialState: S = initState
    override protected val dispatcher: parent.Dispatcher[X, Seq[B]] = parent.dispatcher ~ Reporter[Y]
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


  override def tapEach[U,S](initState: S)(f: (S, Y) ~> (S, U)): RotaryStateMachine[X,Y] = new Relay[X,Y,S] {
    override def initialState: S = initState
    override protected val dispatcher: parent.Dispatcher[X, Y] = parent.dispatcher ~ Reporter[Y]
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

  override def foreach(f: Y => Unit): Unit = ???
}
object RotaryStateMachine{



}
