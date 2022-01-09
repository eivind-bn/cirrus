package object event {

  lazy val Report: EventReporter.type = EventReporter

  lazy val Delegate: EventDelegator.type = EventDelegator

  lazy val Broadcast: EventBroadcast.type = EventBroadcast

}
