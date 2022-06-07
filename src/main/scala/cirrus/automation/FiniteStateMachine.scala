package automation

trait FiniteStateMachine[I,O,S] extends PartialFunction[(I,S),(S,O)] {

  val state: S


  def apply(data: I): (O, FiniteStateMachine[I,O,S]) = {

    val (newState,newData) = this.apply(data -> state)

    val newMachine: FiniteStateMachine[I,O,S] = FiniteStateMachine(newState)(this)

    newData -> newMachine
  }

}
object FiniteStateMachine{

  def apply[I,O,S](initState: S)(transits: PartialFunction[(I,S),(S,O)]): FiniteStateMachine[I,O,S] =
    new FiniteStateMachine[I,O,S] {

      override val state: S = initState

      override def isDefinedAt(x: (I, S)): Boolean = transits.isDefinedAt(x)
      override def apply(v1: (I, S)): (S, O) = transits.apply(v1)
      override def applyOrElse[A1 <: (I, S), B1 >: (S, O)](x: A1, default: A1 => B1): B1 = transits.applyOrElse(x, default)
    }

}
