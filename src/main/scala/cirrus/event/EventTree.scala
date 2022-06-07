package cirrus.event

import cirrus.combinator.{EventStream, PureStream}

trait EventTree[I,O,CC[_,O] <: EventTree[_,O,CC]] extends EventStream[I,O,CC] with PureStream[I,O,CC]
object EventTree{
  
  type Dispatcher[I,O] <: EventTree[I,O,Dispatcher]
  
}