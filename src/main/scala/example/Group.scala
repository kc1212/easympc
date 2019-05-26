package example

import org.bouncycastle.math.ec.custom.djb.{
  Curve25519,
  Curve25519FieldElement,
  Curve25519Point
}
import scala.math.BigInt
import scala.util.Random
import java.math.BigInteger

trait Group[P, S] {
  def add(a: P, b: P): P
  def mul(p: P, s: S): P
  def base(): P
  def rand(r: Random): S
  def negate(p: P): P
}

object Group {
  def apply[P, S](implicit g: Group[P, S]): Group[P, S] = g
  def add[P](a: P, b: P)(implicit g: Group[P, _]): P = g.add(a, b)
  def mul[P, S](p: P, s: S)(implicit g: Group[P, S]): P = g.mul(p, s)
  def base[P]()(implicit g: Group[P, _]): P = g.base()
  def rand[S]()(implicit g: Group[_, S], r: Random): S = g rand r
  def negate[P](p: P)(implicit g: Group[P, _]): P = g negate p

  implicit class BinaryOps[P, S](a: P)(implicit g: Group[P, S]) {
    def ++(b: P): P = Group.add(a, b)
    def **(s: S): P = Group.mul(a, s)
    def negate(): P = Group.negate(a)
  }

  implicit val bigIntGroup: Group[BigInt, BigInt] = {
    new Group[BigInt, BigInt] {
      private val g512 = BigInt(
        "153d5d6172adb43045b68ae8e1de1070b6137005686d29d3d73a7749199681ee5b212c9b96bfdcfa5b20cd5e3fd2044895d609cf9b410b7a0f12ca1cb9a428cc",
        16
      )
      private val p512 = BigInt(
        "9494fec095f3b85ee286542b3836fc81a5dd0a0349b4c239dd38744d488cf8e31db8bcb7d33b41abb9e5a33cca9144b1cef332c94bf0573bf047a3aca98cdf3b",
        16
      )
      override def add(a: BigInt, b: BigInt): BigInt = (a * b) mod p512
      override def mul(p: BigInt, s: BigInt): BigInt = p.modPow(s, p512)
      override def base(): BigInt = g512
      override def rand(r: Random): BigInt = BigInt(p512.bitCount, r) mod p512
      override def negate(p: BigInt): BigInt = p modInverse p512
    }
  }

  type TPt = Curve25519Point
  type TFe = Curve25519FieldElement
  implicit val curve25519: Group[TPt, TFe] = {
    new Group[TPt, TFe] {
      private val curve = new Curve25519()
      private val b = curve
        .createPoint(
          new BigInteger("9"),
          new BigInteger(
            "14781619447589544791020593568409986887264606134616475288964881837755586237401"
          )
        )
        .asInstanceOf[TPt]
      private val multiplier = curve.getMultiplier
      override def add(a: TPt, b: TPt): TPt = a.add(b).asInstanceOf[TPt]
      override def mul(p: TPt, s: TFe): TPt =
        multiplier.multiply(p, s.toBigInteger).asInstanceOf[TPt]
      override def base(): TPt = b
      override def rand(r: Random): TFe = ???
      override def negate(p: TPt): TPt = p.negate().asInstanceOf[TPt]
    }
  }
}
