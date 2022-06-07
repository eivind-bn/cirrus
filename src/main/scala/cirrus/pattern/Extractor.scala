package cirrus.pattern

trait Extractor[A,B]{
  def unapply(arg: A): Option[B]
}

