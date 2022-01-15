package protocol

import io.Serializable

trait Protocol[S] extends Serializable {


  def flatten: Iterable[S]




}
