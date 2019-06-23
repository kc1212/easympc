package eu.cong.easympc

import spire.algebra.{AbGroup, Eq}

import scala.util.Random

trait AbGroupScalar extends AbGroup[BigInt] with Rand[BigInt] with Eq[BigInt] {
  def order: BigInt
  def mul(x: BigInt, y: BigInt): BigInt
}

object AbGroupScalar {

  def fromOrder(ord: BigInt): AbGroupScalar = new AbGroupScalar {
    require(ord.isProbablePrime(32), "not prime")

    override def order: BigInt = ord

    override def inverse(a: BigInt): BigInt = a.modInverse(ord)

    override def empty: BigInt = 1

    // TODO use bitLength or bitCount?
    // TODO is it ok to use Stream?
    override def randElem()(implicit r: Random): BigInt = {
      Stream.continually(BigInt(ord.bitLength, r) mod ord).filter(_ >= empty).head
    }.ensuring(_ >= empty, "generated value cannot be smaller than the empty value")

    override def combine(x: BigInt, y: BigInt): BigInt = (x * y) mod ord

    override def mul(x: BigInt, y: BigInt): BigInt = x.modPow(y, ord)

    override def eqv(x: BigInt, y: BigInt): Boolean = x == y
  }

  implicit val curve25519Scalar: AbGroupScalar = fromOrder(Spec.curve25519.getN)

  implicit class Ops(val a: BigInt)(implicit grp: AbGroupScalar) {
    def negate(): BigInt = grp.inverse(a)
    def div(b: BigInt): BigInt = grp.mul(a, grp.inverse(b))
    def <+>(b: BigInt): BigInt = grp.combine(a, b)
    def <*>(s: BigInt): BigInt = grp.mul(a, s)
    def <->(b: BigInt): BigInt = grp.combine(a, grp.inverse(b))
  }

}
