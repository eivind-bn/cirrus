package io

import combinator.PureStream

import scala.collection.{AbstractIterator, Factory, IterableFactory, mutable}
import scala.concurrent.ExecutionContext

trait InputStream[I, CC[I] <: InputStream[I,CC]] extends Input[I] with PureStream[I,I,[_,X] =>> CC[X]] { self =>
  
  
  
}
