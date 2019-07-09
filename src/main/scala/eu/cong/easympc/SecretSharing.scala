package eu.cong.easympc

import scala.util.Random
import AbGroupScalar.Ops

object SecretSharing {

  case class Share(x: BigInt, y: BigInt)

  def share(secret: BigInt, t: Int, n: Int)(implicit grp: AbGroupScalar, r: Random): Seq[Share] = {
    require(t <= n, "threshold must be lower than n")
    val privateCoeff = List(secret) ++ (1 until t).map { _ =>
      grp.randElem()
    }
    evalAll(n, privateCoeff).ensuring(_.size == n, "evaluation size is wrong")
  }

  def combine(shares: Seq[Share])(implicit grp: AbGroupScalar): BigInt = {
    val xs = shares.map(_.x)
    val ys = shares.map(_.y)
    val x = grp.empty
    ys.indices
      .map { j =>
        ys(j) <*> lagrange_basis(j)(x, xs)
      }
      .foldLeft(grp.empty)(_ <+> _)
  }

  def zeros(t: Int, n: Int, len: Int)(implicit grp: AbGroupScalar, r: Random): Seq[Seq[Share]] =
    (0 to len) map { _ =>
      share(BigInt(0), t, n)
    }

  // evaluate on n points, start at 1 because 0 holds the secret
  // use this function as a deterministic version of share
  private[easympc] def evalAll(n: Int, coeffs: List[BigInt])(implicit grp: AbGroupScalar): Seq[Share] = {
    val xs = (1 to n).map(BigInt(_))
    val ys = xs.map(eval(_, coeffs))
    makeShares(xs, ys)
  }

  // evaluate the polynomial represented by coeffs at the point x using Horner's method
  private[easympc] def eval(x: BigInt, coeffs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    coeffs.reverse.foldLeft(grp.empty) { (accum, coeff) =>
      (accum <*> x) <+> coeff
    }
  }

  private[easympc] def lagrange_basis(j: Int)(x: BigInt, xs: Seq[BigInt])(implicit grp: AbGroupScalar): BigInt = {
    require(0 <= j && j < xs.size)
    xs.indices.filter(_ != j).foldLeft(BigInt(1)) { (accum, m) =>
      accum <*> ((x <-> xs(m)) </> (xs(j) <-> xs(m)))
    }
  }

  private def makeShares[S](xs: Seq[BigInt], ys: Seq[BigInt]): Seq[Share] = {
    require(xs.size == ys.size)
    (xs zip ys).map { x =>
      Share(x._1, x._2)
    }
  }
}
