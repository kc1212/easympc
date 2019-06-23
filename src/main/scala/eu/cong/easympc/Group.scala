package eu.cong.easympc

import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}
import org.bouncycastle.util.BigIntegers

import scala.math.BigInt
import scala.util.Random

trait Group[P, S] {
  def add(a: P, b: P): P
  def mul(p: P, s: S): P
  def negate(p: P): P

  def scalarAdd(a: S, b: S): S
  def scalarMul(a: S, b: S): S
  def scalarDiv(a: S, b: S): S
  def scalarNeg(a: S): S
  def fromBigInt(x: BigInt): S
  def rand(r: Random): S

  val order: S
  val zero: S
  val one: S
  val base: P
  val inf: P
}

object Group {
  def apply[P, S](implicit g: Group[P, S]): Group[P, S] = g

  def add[P](a: P, b: P)(implicit g: Group[P, _]): P = g.add(a, b)
  def mul[P, S](p: P, s: S)(implicit g: Group[P, S]): P = g.mul(p, s)
  def neg[P](p: P)(implicit g: Group[P, _]): P = g negate p
  def base[P]()(implicit g: Group[P, _]): P = g.base

  def scalarAdd[S](a: S, b: S)(implicit g: Group[_, S]): S = g.scalarAdd(a, b)
  def scalarMul[S](a: S, b: S)(implicit g: Group[_, S]): S = g.scalarMul(a, b)
  def scalarDiv[S](a: S, b: S)(implicit g: Group[_, S]): S = g.scalarDiv(a, b)
  def scalarNeg[S](a: S)(implicit g: Group[_, S]): S = g.scalarNeg(a)
  def rand[S]()(implicit g: Group[_, S], r: Random): S = g rand r

  implicit class PointOps[P, S](val a: P)(implicit g: Group[P, S]) {
    def negate(): P = Group.neg(a)
    def ++(b: P): P = Group.add(a, b)
    def **(s: S): P = Group.mul(a, s)
    def --(b: P): P = Group.add(a, Group.neg(b))
  }

  implicit class ScalarOps[S](a: S)(implicit g: Group[_, S]) {
    def +++(b: S): S = Group.scalarAdd(a, b)
    def ***(b: S): S = Group.scalarMul(a, b)
    def ---(b: S): S = Group.scalarAdd(a, Group.scalarNeg(b))
    def div(b: S): S = Group.scalarDiv(a, b)
  }

  def customBigIntGroup(g: BigInt, p: BigInt): Group[BigInt, BigInt] = {
    new Group[BigInt, BigInt] {
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

  implicit val bigIntGroup: Group[BigInt, BigInt] = {
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
  implicit val curve25519Group: Group[TPt, BigInt] = {
    new Group[TPt, BigInt] {
      private val spec = Spec.curve25519
      private val curve = spec.getCurve

      override def add(a: TPt, b: TPt): TPt = a.add(b)
      override def mul(p: TPt, s: BigInt): TPt =
        curve.getMultiplier.multiply(p, s.bigInteger)
      override def negate(p: TPt): TPt = p.negate()

      override def scalarAdd(a: BigInt, b: BigInt): BigInt = a + b
      override def scalarMul(a: BigInt, b: BigInt): BigInt = a * b
      override def scalarNeg(a: BigInt): BigInt = a.negate()
      override def scalarDiv(a: BigInt, b: BigInt): BigInt = (a * b.modInverse(order)).mod(order)
      override def fromBigInt(x: BigInt): BigInt = x.bigInteger
      override def rand(r: Random): BigInt = BigInt(curve.getFieldSize, r) mod spec.getN

      override val order: BigInt = spec.getN
      override val zero: BigInt = BigIntegers.ZERO
      override val base: TPt = spec.getG
      override val inf: TPt = curve.getInfinity
      override val one: BigInt = BigIntegers.ONE
    }
  }
}
