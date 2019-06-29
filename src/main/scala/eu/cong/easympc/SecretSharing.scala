package eu.cong.easympc

import scala.util.Random
import AbGroupScalar.Ops

object SecretSharing {

  case class XYShare(x: BigInt, y: BigInt)

  object XYShare {
    def apply[S](xs: Seq[BigInt], ys: Seq[BigInt]): Seq[XYShare] = {
      require(xs.size == ys.size)
      (xs zip ys).map { x =>
        XYShare(x._1, x._2)
      }
    }
  }

  def share(secret: BigInt, t: Int, n: Int)(implicit grp: AbGroupScalar, r: Random): Seq[XYShare] = {
    require(t <= n)
    // generate t coefficients
    val privateCoeff = List(secret) ++ (1 until t) map { _ =>
      grp.randElem()
    }

    // evaluate on n points, start at 1 because 0 holds the secret
    val xs = (1 to n).map(BigInt(_))
    val ys = xs.map(eval(_, privateCoeff))
    XYShare(xs, ys)
  }

  def combine(shares: Seq[XYShare])(implicit grp: AbGroupScalar): BigInt = {
    val xs = shares.map(_.x)
    val ys = shares.map(_.y)
    val x = grp.empty
    ys.indices
      .map { j =>
        ys(j) <*> lagrange_basis(j)(x, xs)
      }
      .foldLeft(grp.empty)(_ <+> _)
  }

  private def eval(x: BigInt, coeffs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    // evaluate the polynomial represented by coeffs at the point x using Horner's method
    coeffs.reverse.foldLeft(grp.empty) { (accum, coeff) =>
      (accum <*> x) <+> coeff
    }
  }

  private def lagrange_basis(j: Int)(x: BigInt, xs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    // var accum = BigInt(1)
    require(0 <= j && j < xs.size)
    xs.indices.filter(_ != j).foldLeft(BigInt(1)) { (accum, m) =>
      accum <*> ((x <-> xs(m)) </> (xs(j) <-> xs(m)))
    }
    /*
    for (m <- xs.indices) {
      if (m != j) {
        accum =
      }
    }
    accum

   */
  }
}
