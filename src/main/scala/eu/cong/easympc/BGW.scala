package eu.cong.easympc

import eu.cong.easympc.SecretSharing.Share

/**
  * BGW initiates a player of the BGW protocol. During initiation, all necessary information for computing the final
  * share must be given.
  *
  * @param id
  * @param roster
  * @param prog
  * @param shares
  * @param zeros
  * @tparam T
  */
case class BGW[T](id: T, roster: Set[T], prog: Any, shares: Map[T, Share], zeros: Iterable[Map[T, Share]]) {
  def compute(): Either[BigInt, String] = {
    require(roster contains id)
    ???
  }
}
