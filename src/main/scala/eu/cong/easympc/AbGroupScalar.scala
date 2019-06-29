package eu.cong.easympc

import spire.algebra.{AbGroup, Eq}

import scala.util.Random

trait AbGroupScalar extends AbGroup[BigInt] with Rand[BigInt] with Eq[BigInt] {
  def order: BigInt
  def mul(x: BigInt, y: BigInt): BigInt
}

object AbGroupScalar {

  def additiveGroupFromOrder(ord: BigInt): AbGroupScalar = new AbGroupScalar {
    require(ord.isProbablePrime(32), ord + " is not a prime")

    override def order: BigInt = ord

    override def inverse(a: BigInt): BigInt = (-a) mod ord

    override def empty: BigInt = 0

    // TODO use bitLength or bitCount?
    // TODO is it ok to use LazyList?
    override def randElem()(implicit r: Random): BigInt = {
      LazyList.continually(BigInt(ord.bitLength, r) mod ord).filter(_ >= empty).head
    }.ensuring(_ >= empty, "generated value cannot be smaller than the empty value")

    override def combine(x: BigInt, y: BigInt): BigInt = (x + y) mod ord

    override def mul(x: BigInt, y: BigInt): BigInt = (x * y) mod ord

    override def eqv(x: BigInt, y: BigInt): Boolean = x == y
  }

  // TODO this is not right because it's vulnerable to the sub group attack
  implicit val curve25519Scalar: AbGroupScalar = additiveGroupFromOrder(Spec.curve25519.getN)

  implicit class Ops(val a: BigInt)(implicit grp: AbGroupScalar) {
    def negate: BigInt = grp.inverse(a)
    def </>(b: BigInt): BigInt = grp.mul(a, b.modInverse(grp.order))
    def <+>(b: BigInt): BigInt = grp.combine(a, b)
    def <*>(b: BigInt): BigInt = grp.mul(a, b)
    def <->(b: BigInt): BigInt = grp.combine(a, grp.inverse(b))
  }

}
