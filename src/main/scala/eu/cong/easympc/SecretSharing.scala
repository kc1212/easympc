package eu.cong.easympc

import scala.util.Random
import AbGroupScalar.Ops

object SecretSharing {

  case class XYShare(x: BigInt, y: BigInt)
  object XYShare {
    def fromTuples[S](tuples: Seq[(BigInt, BigInt)]): Seq[XYShare] = {
      tuples.map { x =>
        XYShare(x._1, x._2)
      }
    }
  }

  def share(secret: BigInt, t: Int, n: Int)(implicit grp: AbGroupScalar, r: Random): Seq[XYShare] = {
    require(t <= n)
    // generate t coefficients
    val privateCoeff = List(secret) ++ (1 until t) map {
      _ => grp.randElem()
    }

    // evaluate on n points, start at 1 because 0 holds the secret
    val xs = (1 to n).map(BigInt(_))
    val ys = xs.map(eval(_, privateCoeff))
    XYShare.fromTuples(xs zip ys)
  }

  def combine(shares: Seq[XYShare])(implicit grp: AbGroupScalar): BigInt = {
    val xs = shares.map(_.x)
    val ys = shares.map(_.y)
    val x = grp.empty
    ys.zipWithIndex.map {
        case (y: BigInt, j: Int) => y <*> lagrange_basis(j)(x, xs)
      }
      .foldLeft(grp.empty)(_ <+> _)
  }

  private def eval(x: BigInt, coeffs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    // evaluate the polynomial represented by coeffs at the point x using Horner's method
    coeffs.reverse.foldLeft(grp.order) { (a, b) =>
      a <*> x <+> b
    }
  }

  private def lagrange_basis(j: Int)(x: BigInt, xs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    var accum = grp.empty
    if (j < 0 || j >= xs.size) {
      throw new IllegalArgumentException
    }
    for (m <- xs.indices) {
      if (m != j) {
        accum = accum <*> ((x <-> xs(m)) div (xs(j) <-> xs(m)))
      }
    }
    accum
  }
}
