package eu.cong.easympc

import org.bouncycastle.math.ec.{ECFieldElement, ECPoint}

import scala.math.BigInt
import scala.util.Random

trait Group[P, S] {
  def add(a: P, b: P): P
  def mul(p: P, s: S): P
  def rand(r: Random): S
  def negate(p: P): P

  val order: S
  val base: P
  val inf: P
}

object Group {
  def apply[P, S](implicit g: Group[P, S]): Group[P, S] = g
  def add[P](a: P, b: P)(implicit g: Group[P, _]): P = g.add(a, b)
  def mul[P, S](p: P, s: S)(implicit g: Group[P, S]): P = g.mul(p, s)
  def base[P]()(implicit g: Group[P, _]): P = g.base
  def rand[S]()(implicit g: Group[_, S], r: Random): S = g rand r
  def negate[P](p: P)(implicit g: Group[P, _]): P = g negate p

  implicit class GroupOps[P, S](a: P)(implicit g: Group[P, S]) {
    def negate(): P = Group.negate(a)
    def ++(b: P): P = Group.add(a, b)
    def **(s: S): P = Group.mul(a, s)
    def --(b: P): P = Group.add(a, Group.negate(b))
  }

  private def customBigIntGroup(g: BigInt, p: BigInt): Group[BigInt, BigInt] = {
    new Group[BigInt, BigInt] {
      override def add(x: BigInt, y: BigInt): BigInt = (x * y) mod p
      override def mul(x: BigInt, s: BigInt): BigInt = x.modPow(s, p)
      override def rand(r: Random): BigInt = BigInt(p.bitCount, r) mod p
      override def negate(x: BigInt): BigInt = x modInverse p

      override val order: BigInt = p
      override val base: BigInt = g
      override val inf: BigInt = 0
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
  type TFe = ECFieldElement
  implicit val curve25519Group: Group[TPt, TFe] = {
    new Group[TPt, TFe] {
      private val spec = Spec.curve25519
      private val curve = spec.getCurve

      override def add(a: TPt, b: TPt): TPt = a.add(b)
      override def mul(p: TPt, s: TFe): TPt =
        curve.getMultiplier.multiply(p, s.toBigInteger)
      override def rand(r: Random): TFe = {
        val x = BigInt(curve.getFieldSize, r) mod spec.getN
        curve.fromBigInteger(x.bigInteger)
      }
      override def negate(p: TPt): TPt = p.negate()

      override val order: TFe = curve.fromBigInteger(spec.getN)
      override val base: TPt = spec.getG
      override val inf: TPt = curve.getInfinity
    }
  }
}
