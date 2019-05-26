package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.{Arbitrary, Gen}
import org.scalatest._
import org.scalatest.prop.Checkers

import scala.util.Random

class GroupSpec extends FlatSpec with Checkers {
  import Group.{GroupOps, base}
  val r: Random = new Random(new SecureRandom())

  def elgamal[P, S](msg: P, x: S, y: S)(implicit g: Group[P, S]): Boolean = {
    // key gen
    val h = base ** x

    // encryption
    val s = h ** y
    val c1 = base ** y
    val c2 = msg ++ s

    // decryption
    val s_receiver = c1 ** x
    val msg_receiver = c2 -- s_receiver
    msg_receiver == msg
  }

  def dh[P, S](x: S, y: S)(implicit g: Group[P, S]): Boolean = {
    val X = base ** x
    val Y = base ** y
    Y ** x == X ** y
  }

  implicit def arbScalar(
    implicit g: Group[_, Group.TFe]
  ): Arbitrary[Group.TFe] = Arbitrary {
    Gen.resultOf((_: Int) => g.rand(r))
  }

  "curve25519 group" should "correctly run ElGamal" in {
    import Group.curve25519
    check(
      (msg: Group.TFe, x: Group.TFe, y: Group.TFe) => elgamal(base ** msg, x, y)
    )
  }

  it should "correctly run DH" in {
    import Group.curve25519
    check((x: Group.TFe, y: Group.TFe) => dh(x, y))
  }

  "big int group" should "correctly run ElGamal" in {
    import Group.bigIntGroup
    check((msg: BigInt, x: BigInt, y: BigInt) => elgamal(base ** msg, x, y))
  }

  it should "correctly run DH" in {
    import Group.bigIntGroup
    check((x: BigInt, y: BigInt) => dh(x, y))
  }

  it should "correctly run DH with test vector" in {
    // TODO use test vectors in https://tools.ietf.org/html/rfc7748#page-14
  }
}
