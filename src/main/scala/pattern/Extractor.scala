package pattern


trait Extractor[A,B]{
  def unapply(arg: A): Option[B]
}

