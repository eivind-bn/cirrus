package automation

import event.{Broadcaster, Delegator, Event, Reporter}
import combinator.ImpureStream

trait StateMachine[I,O,CC[I,O] <: Event[I,O,CC]]
  extends Event[I,O,[X,Y] =>> StateMachine[X,Y,CC]] with ImpureStream[I,O,[X,Y] =>> StateMachine[X,Y,CC]]
