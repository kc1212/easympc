package eu.cong.easympc

import org.bouncycastle.math.ec.ECPoint
import spire.algebra.{AbGroup, Eq}

import scala.util.Random

trait AbGroupPoint[P] extends AbGroup[P] with Rand[P] with Eq[P] {
  def scalarMul(p: P, x: BigInt): P
  def order: BigInt
  def base: P
}

object AbGroupPoint {

  def add[P](a: P, b: P)(implicit g: AbGroupPoint[P]): P = g.combine(a, b)
  def mul[P](p: P, s: BigInt)(implicit g: AbGroupPoint[P]): P = g.scalarMul(p, s)
  def neg[P](p: P)(implicit g: AbGroupPoint[P]): P = g inverse p
  def base[P]()(implicit g: AbGroupPoint[P]): P = g.base

  implicit val curve25519Point: AbGroupPoint[ECPoint] = new AbGroupPoint[ECPoint] {
    private val spec = Spec.curve25519
    private val curve = spec.getCurve

    override def scalarMul(p: ECPoint, x: BigInt): ECPoint =
      curve.getMultiplier.multiply(p, x.bigInteger)

    override def order: BigInt = spec.getN

    override def base: ECPoint = spec.getG

    override def inverse(a: ECPoint): ECPoint = a.negate()

    override def empty: ECPoint = curve.getInfinity

    override def randElem()(implicit r: Random): ECPoint = ???

    override def combine(x: ECPoint, y: ECPoint): ECPoint = x.add(y)

    override def eqv(x: ECPoint, y: ECPoint): Boolean = x.equals(y)
  }

  implicit val bigIntGroup: AbGroupPoint[BigInt] = {
    val g512 = BigInt(
      "153d5d6172adb43045b68ae8e1de1070b6137005686d29d3d73a7749199681ee5b212c9b96bfdcfa5b20cd5e3fd2044895d609cf9b410b7a0f12ca1cb9a428cc",
      16
    )
    val p512 = BigInt(
      "9494fec095f3b85ee286542b3836fc81a5dd0a0349b4c239dd38744d488cf8e31db8bcb7d33b41abb9e5a33cca9144b1cef332c94bf0573bf047a3aca98cdf3b",
      16
    )
    customBigIntGroup(g512, p512)
  }

  def customBigIntGroup(generator: BigInt, prime: BigInt): AbGroupPoint[BigInt] =
    new AbGroupPoint[BigInt] {
      require(prime.isProbablePrime(32))
      require(generator < prime)

      override def scalarMul(p: BigInt, x: BigInt): BigInt = p.modPow(x, prime)

      override def order: BigInt = prime

      override def base: BigInt = generator

      override def inverse(a: BigInt): BigInt = a.modInverse(prime)

      override def empty: BigInt = 1 // because prime integer group is multiplicative

      override def randElem()(implicit r: Random): BigInt = ???

      override def combine(x: BigInt, y: BigInt): BigInt = (x * y) mod prime

      override def eqv(x: BigInt, y: BigInt): Boolean = x == y
    }

  implicit class Ops[P](val a: P)(implicit grp: AbGroupPoint[P]) {
    def negate(): P = AbGroupPoint.neg(a)
    def |+|(b: P): P = AbGroupPoint.add(a, b)
    def |*|(s: BigInt): P = AbGroupPoint.mul(a, s)
    def |-|(b: P): P = AbGroupPoint.add(a, AbGroupPoint.neg(b))
  }

}
