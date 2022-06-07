package cirrus.automation

import cirrus.combinator.{EventStream, ImpureStream}

trait StateMachine[X,Y,CC[_,Y] <: StateMachine[_,Y,CC]] extends EventStream[X,Y,CC] with ImpureStream[X,Y,CC]
