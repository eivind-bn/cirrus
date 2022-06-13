package cirrus.automation

import cirrus.combinator.{ModularStream, ImpureStream}
import cirrus.event.EventTree

trait StateMachine[X,Y,CC[_,Y] <: StateMachine[_,Y,CC]] extends ModularStream[X,Y,CC] with ImpureStream[X,Y,CC]
object StateMachine{

  trait Factory[CC[_,O] <: EventTree[_,O,CC],DD[_,Y] <: StateMachine[_,Y,DD]]{ self =>
    def of[T]: DD[T,T]{type Dispatcher[X,Y] = CC[X,Y]}
  }

}
