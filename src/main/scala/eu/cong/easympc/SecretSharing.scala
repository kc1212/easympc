package eu.cong.easympc

import scala.util.Random

import Group._

object SecretSharing {
  def share[S](secret: Option[S], t: Int, n: Int)(implicit g: Group[_, S],
                                                  r: Random): Seq[(S, S)] = {
    // generate t coefficients
    val coeff0: S = secret match {
      case Some(v) => v
      case None    => g.rand(r)
    }
    val privateCoeff = List(coeff0) ++ (1 until t).map(_ => g.rand(r))

    // evaluate on n points, start at 1 because 0 holds the secret
    val xs = (1 to n).map(g.fromBigInt(_))
    val ys = xs.map(eval(_, privateCoeff))
    xs zip ys
  }

  def combine[S](shares: Seq[(S, S)])(implicit g: Group[_, S]): S = {
    val xs = shares.map(_._1)
    val ys = shares.map(_._2)
    val x = g.zero
    ys.zipWithIndex
      .map {
        case (y, j) => y *** lagrange_basis(j)(x, xs)
      }
      .foldLeft(g.zero)(_ +++ _)
  }

  private def eval[_, S](x: S, coeffs: Seq[S])(implicit g: Group[_, S]): S = {
    // evaluate the polynomial represented by coeffs at the point x using Horner's method
    coeffs.reverse.foldLeft(g.order) { (a, b) =>
      a *** x +++ b
    }
  }

  private def lagrange_basis[S](j: Int)(x: S, xs: Seq[S])(implicit g: Group[_, S]): S = {
    var accum = g.one
    if (j < 0 || j >= xs.size) {
      throw new IllegalArgumentException
    }
    for (m <- xs.indices) {
      if (m != j) {
        accum = accum *** ((x --- xs(m)) div (xs(j) --- xs(m)))
      }
    }
    accum
  }
}
