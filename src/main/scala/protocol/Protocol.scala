package protocol

trait Protocol[S] extends Serializable {


  def flatten: Iterable[S]




}
