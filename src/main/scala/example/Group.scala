package example

import org.bouncycastle.math.ec.custom.djb.{Curve25519Point, Curve25519FieldElement}
import org.bouncycastle.math.ec.MontgomeryLadderMultiplier
import scala.math.BigInt

trait Group[P, S] {
  def add(a: P, b: P): P
  def mul(p: P, s: S): P
  def generator(): P
  def rand(r: Rand): S
  def inv(p: P): P
}

object Group {
  def apply[P, S](implicit g: Group[P, S]): Group[P, S] = g
  def add[P](a: P, b: P)(implicit g: Group[P, _]): P = g.add(a, b)
  def mul[P, S](p: P, s: S)(implicit g: Group[P, S]): P = g.mul(p, s)
  def generator[P]()(implicit g: Group[P, _]): P = g.generator
  def rand[S]()(implicit g: Group[_, S], r: Rand): S = g rand r
  def inv[P](p: P)(implicit g: Group[P, _]): P = g inv p

  implicit class BinaryOps[P, S](a: P)(implicit g: Group[P, S]) {
    def ++(b: P): P = Group.add(a, b)
    def **(s: S): P = Group.mul(a, s)
    def inv(): P = Group.inv(a)
  }

  implicit val bigIntGroup: Group[BigInt, BigInt] = {
    new Group[BigInt, BigInt] {
      val g512 = BigInt("153d5d6172adb43045b68ae8e1de1070b6137005686d29d3d73a7749199681ee5b212c9b96bfdcfa5b20cd5e3fd2044895d609cf9b410b7a0f12ca1cb9a428cc", 16)
      val p512 = BigInt("9494fec095f3b85ee286542b3836fc81a5dd0a0349b4c239dd38744d488cf8e31db8bcb7d33b41abb9e5a33cca9144b1cef332c94bf0573bf047a3aca98cdf3b", 16)
      override def add(a: BigInt, b: BigInt): BigInt = (a * b) mod p512
      override def mul(p: BigInt, s: BigInt): BigInt = p.modPow(s, p512)
      override def generator(): BigInt = g512
      override def rand(r: Rand): BigInt = {
        r.nextInt
      }
      override def inv(p: BigInt): BigInt = p modInverse p512
    }
  }
}
