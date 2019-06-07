package eu.cong.easympc

import scala.util.Random

object Pedersen {
  import Group.{rand, PointOps}

  case class Params[P](g: P, h: P)
  def initialize[P]()(implicit g: Group[P, _], r: Random): Params[P] = {
    ???
  }

  def commit[P, S](secret: S, params: Params[P])(implicit g: Group[P, S], r: Random): (P, S) = {
    val randomness = rand()
    ((params.g ** secret) ++ (params.h ** randomness), randomness)
  }

  def verify[P, S](params: Params[P], commitment: P, secret: S, randomness: S)(implicit g: Group[P, S]): Boolean = {
    commitment == (params.g ** secret) ++ (params.h ** randomness)
  }
}
