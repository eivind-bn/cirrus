package io

trait Serializable extends Equals {


  def serialize: Array[Byte]


  override def canEqual(that: Any): Boolean = that.isInstanceOf[Serializable]


  override def equals(obj: Any): Boolean = obj match {
    case other: Serializable => other.canEqual(this) && (other.serialize sameElements this.serialize)
    case _ => false
  }


  override def hashCode(): Int = serialize.iterator.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)


  override def toString: String = serialize match {
    case binary if binary.length > 30 =>
      s"Serializable(" +
        s"${binary.take(15).map(_.toInt.toHexString).mkString(",")}" +
        s",...," +
        s"${binary.takeRight(15).map(_.toInt.toHexString).mkString(",")}" +
        s")"

    case binary =>
      s"Serializable(" +
        s"${binary.map(_.toInt.toHexString).mkString(",")}" +
        s")"
  }

}
object Serializable{

  trait Serializer[T]{
    def serialize(that: T): Array[Byte]
  }

  given [T <: Serializable]: Serializer[T] = (that: T) => that.serialize

}
