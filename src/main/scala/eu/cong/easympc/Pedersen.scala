package eu.cong.easympc

import scala.util.Random


object Pedersen {
  case class Params[P](g: P, h: P)

  def initialize[P]()(implicit g: AbGroupPoint[P], r: Random): Params[P] = {
    ???
  }

  def commit[P](secret: BigInt, params: Params[P])(implicit suite: AbGroupSuite[P], r: Random): (P, BigInt) = {
    import AbGroupPoint.Ops
    implicit val grp: AbGroupPoint[P] = suite.pointGroup

    val randomness = suite.scalarGroup.randElem()
    ((params.g |*| secret) |+| (params.h |*| randomness), randomness)
  }

  def verify[P](params: Params[P], commitment: P, secret: BigInt, randomness: BigInt)(implicit grp: AbGroupPoint[P]): Boolean = {
    import AbGroupPoint.Ops
    commitment == ((params.g |*| secret) |+| (params.h |*| randomness))
  }
}
