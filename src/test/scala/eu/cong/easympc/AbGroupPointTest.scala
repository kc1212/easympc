package eu.cong.easympc

import AbGroupPoint.Ops

import org.bouncycastle.math.ec.ECPoint
import org.scalacheck.Gen
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.{Checkers, ScalaCheckDrivenPropertyChecks}
import spire.laws.GroupLaws

class AbGroupPointTest extends WordSpec with ScalaCheckDrivenPropertyChecks with Checkers {

  def elgamal[P](msg: P, x: BigInt, y: BigInt)(implicit grp: AbGroupPoint[P]): Boolean = {
    // key gen
    val h = grp.base |*| x

    // encryption
    val s = h |*| y
    val c1 = grp.base |*| y
    val c2 = msg |+| s

    // decryption
    val s_receiver = c1 |*| x
    val msg_receiver = c2 |-| s_receiver
    msg_receiver == msg
  }

  def dh[P](x: BigInt, y: BigInt)(implicit grp: AbGroupPoint[P]): Boolean = {
    val X = grp.base |*| x
    val Y = grp.base |*| y
    (Y |*| x) == (X |*| y)
  }

  "a big integer group" should {
    implicit val grp = AbGroupPoint.bigIntGroup
    implicit val arb = ArbitraryHelper.arbPoint(grp)

    "run elgamal" in
      forAll {
        (msg: BigInt, x: BigInt, y: BigInt) => elgamal(grp.base |*| msg, x, y)
      }
    "run DH" in
      forAll {
        (x: BigInt, y: BigInt) => dh(x, y)
      }
    "ensure scalar mul is equivalent to repeated addition" in {
      val smallInts = for (n <- Gen.choose(1, 1000)) yield n
      forAll (smallInts) { x =>
        (grp.base |*| BigInt(x)) == grp.combineAll(List.fill(x)(grp.base))
      }
    }
    val ruleSet = GroupLaws[BigInt].abGroup
    for ((id, prop) <- ruleSet.all.properties) {
      "ensure " + id in {
        check(prop)
      }
    }
  }

  "a curve25519 point group" should {
    implicit val grp = AbGroupPoint.curve25519Point
    implicit val arb = ArbitraryHelper.arbPoint(grp)

    "run elgamal" in
      forAll {
        (msg: BigInt, x: BigInt, y: BigInt) => elgamal(grp.base |*| msg, x, y)
      }
    "run DH" in
      forAll {
        (x: BigInt, y: BigInt) => dh(x, y)
      }
    "ensure scalar mul is equivalent to repeated addition" in {
      val smallInts = for (n <- Gen.choose(1, 1000)) yield n
      forAll (smallInts) { x =>
        (grp.base |*| BigInt(x)) == grp.combineAll(List.fill(x)(grp.base))
      }
    }
    val ruleSet = GroupLaws[ECPoint].abGroup
    for ((id, prop) <- ruleSet.all.properties) {
      "ensure " + id in {
        check(prop)
      }
    }
  }
}
