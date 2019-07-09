package eu.cong.easympc

import eu.cong.easympc.SecretSharing.Share
import eu.cong.easympc.StackMachine._
import eu.cong.easympc.AbGroupScalar.Ops

import scala.util.Try

/**
  * BGW initiates a player of the BGW protocol. During initiation, all necessary information for computing the final
  * share must be given.
  */
object BGW {

  def apply[T](id: T, roster: Set[T], prog: List[Instruction[T]], shares: Map[T, Share], zeros: Iterable[Map[T, Share]]): Try[Share] = {
    require(roster contains id)
    require(shares.values.map(_.x).toSet.size == 1)
    StackMachine(transformProg(prog, shares), addOp, mulOp)
  }

  private val addOp = (a: Share, b: Share) => {
    require(a.x == b.x, s"${a.x} not equal to ${b.x}")
    Share(a.x, a.y <+> b.y)
  }

  private val mulOp = (a: Share, b: Share) => {
    require(a.x == b.x, s"${a.x} not equal to ${b.x}")
    Share(a.x, a.y <*> b.y)
  }

  private def transformProg[T](prog: List[Instruction[T]], shares: Map[T, Share]): List[Instruction[Share]] =
    prog map {
      case Push(x) => Push(shares(x))
      case Add()   => Add()
      case Mul()   => Mul()
    }
}
