package io

import combinator.PureStream
import event.Reporter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

trait OutputStream[O,CC[O] <: OutputStream[O,CC]] extends Output[O] with PureStream[O,O,[_,X] =>> CC[X]] {



}