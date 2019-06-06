package eu.cong.easympc

import scala.util.Random

import Group._

object SecretSharing {

  case class XYShare[S](x: S, y: S)
  object XYShare {
    def fromTuples[S](tuples: Seq[(S, S)]): Seq[XYShare[S]] = {
      tuples.map { x =>
        XYShare(x._1, x._2)
      }
    }
  }

  def share[S](secret: S, t: Int, n: Int)(implicit g: Group[_, S], r: Random): Seq[XYShare[S]] = {
    if (t > n) {
      throw new IllegalArgumentException
    }
    // generate t coefficients
    val privateCoeff = List(secret) ++ (1 until t).map(_ => g.rand(r))

    // evaluate on n points, start at 1 because 0 holds the secret
    val xs = (1 to n).map(g.fromBigInt(_))
    val ys = xs.map(eval(_, privateCoeff))
    XYShare.fromTuples(xs zip ys)
  }

  def combine[S](shares: Seq[XYShare[S]])(implicit g: Group[_, S]): S = {
    val xs = shares.map(_.x)
    val ys = shares.map(_.y)
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
