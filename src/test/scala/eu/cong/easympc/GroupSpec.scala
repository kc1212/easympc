package eu.cong.easympc

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers

class GroupSpec extends FlatSpec with Checkers {
  import Group.{PointOps, base}

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

  import ArbitraryHelper._

  "curve25519 group" should "correctly run ElGamal" in {
    import Group.curve25519Group
    check((msg: Group.TFe, x: Group.TFe, y: Group.TFe) => elgamal(base ** msg, x, y))
  }

  it should "correctly run DH" in {
    import Group.curve25519Group
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
