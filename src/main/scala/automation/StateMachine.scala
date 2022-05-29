package automation

import event.{Broadcaster, Delegator, EventStream, Reporter}
import combinator.ImpureStream

trait StateMachine[I,O,CC[I,O] <: EventStream[I,O,CC]]
  extends EventStream[I,O,[X,Y] =>> StateMachine[X,Y,CC]] with ImpureStream[I,O,[X,Y] =>> StateMachine[X,Y,CC]]
