package eu.cong.easympc

import org.bouncycastle.math.ec.ECPoint
import org.bouncycastle.util.BigIntegers

import scala.util.Random

trait Group[P] {
  def add(a: P, b: P): P
  def mul(p: P, s: BigInt): P
  def negate(p: P): P

  def scalarAdd(a: BigInt, b: BigInt): BigInt
  def scalarMul(a: BigInt, b: BigInt): BigInt
  def scalarDiv(a: BigInt, b: BigInt): BigInt
  def scalarNeg(a: BigInt): BigInt
  def fromBigInt(x: BigInt): BigInt
  def rand(r: Random): BigInt

  val order: BigInt
  val zero: BigInt
  val one: BigInt
  val base: P
  val inf: P
}

object Group {
  /*
  def apply[P](implicit g: Group[P]): Group[P] = g

  def add[P](a: P, b: P)(implicit g: Group[P]): P = g.add(a, b)
  def mul[P](p: P, s: BigInt)(implicit g: Group[P]): P = g.mul(p, s)
  def neg[P](p: P)(implicit g: Group[P]): P = g negate p
  def base[P]()(implicit g: Group[P]): P = g.base

  def scalarAdd(a: BigInt, b: BigInt)(implicit g: Group[_]): BigInt = g.scalarAdd(a, b)
  def scalarMul(a: BigInt, b: BigInt)(implicit g: Group[_]): BigInt = g.scalarMul(a, b)
  def scalarDiv(a: BigInt, b: BigInt)(implicit g: Group[_]): BigInt = g.scalarDiv(a, b)
  def scalarNeg(a: BigInt)(implicit g: Group[_]): BigInt = g.scalarNeg(a)
  def rand()(implicit g: Group[_], r: Random): BigInt = g rand r

  implicit class PointOps[P](val a: P)(implicit g: Group[P]) {
    def negate(): P = Group.neg(a)
    def ++(b: P): P = Group.add(a, b)
    def **(s: BigInt): P = Group.mul(a, s)
    def --(b: P): P = Group.add(a, Group.neg(b))
  }

  implicit class ScalarOps[P](a: BigInt)(implicit g: Group[P]) {
    def +++(b: BigInt): BigInt = Group.scalarAdd(a, b)
    def ***(b: BigInt): BigInt = Group.scalarMul(a, b)
    def ---(b: BigInt): BigInt = Group.scalarAdd(a, Group.scalarNeg(b))
    def div(b: BigInt): BigInt = Group.scalarDiv(a, b)
  }

  def customBigIntGroup(g: BigInt, p: BigInt): Group[BigInt] = {
    new Group[BigInt] {
      override def add(x: BigInt, y: BigInt): BigInt = (x * y) mod p
      override def mul(x: BigInt, s: BigInt): BigInt = x.modPow(s, p)
      override def negate(x: BigInt): BigInt = x modInverse p

      override def scalarAdd(a: BigInt, b: BigInt): BigInt = (a + b) mod p
      override def scalarMul(a: BigInt, b: BigInt): BigInt = (a * b) mod p
      override def scalarNeg(a: BigInt): BigInt = -a mod p
      override def scalarDiv(a: BigInt, b: BigInt): BigInt = a * b.modInverse(p) mod p
      override def fromBigInt(x: BigInt): BigInt = x
      override def rand(r: Random): BigInt = BigInt(p.bitCount, r) mod p

      override val order: BigInt = p
      override val zero: BigInt = BigIntegers.ZERO
      override val base: BigInt = g
      override val inf: BigInt = 0
      override val one: BigInt = BigIntegers.ONE
    }
  }

  implicit val bigIntGroup: Group[BigInt] = {
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

  type TPt = ECPoint
  implicit val curve25519Group: Group[TPt] = {
    new Group[TPt] {
      private val spec = Spec.curve25519
      private val curve = spec.getCurve

      override def add(a: TPt, b: TPt): TPt = a.add(b)
      override def mul(p: TPt, s: BigInt): TPt =
        curve.getMultiplier.multiply(p, s.bigInteger)
      override def negate(p: TPt): TPt = p.negate()

      override def scalarAdd(a: BigInt, b: BigInt): BigInt = a + b
      override def scalarMul(a: BigInt, b: BigInt): BigInt = a * b
      override def scalarNeg(a: BigInt): BigInt = a.negate()
      override def scalarDiv(a: BigInt, b: BigInt): BigInt = (a * b.modInverse(order)) mod order
      override def fromBigInt(x: BigInt): BigInt = x.bigInteger
      override def rand(r: Random): BigInt = BigInt(curve.getFieldSize, r) mod spec.getN

      override val order: BigInt = spec.getN
      override val zero: BigInt = BigIntegers.ZERO
      override val base: TPt = spec.getG
      override val inf: TPt = curve.getInfinity
      override val one: BigInt = BigIntegers.ONE
    }
  }
   */
}
