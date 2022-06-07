package cirrus.io

import cirrus.event.Delegator


trait Input[T] {

  val onData: Delegator[T]

  val onOpen: Delegator[Unit]

  val onClose: Delegator[Unit]

}
