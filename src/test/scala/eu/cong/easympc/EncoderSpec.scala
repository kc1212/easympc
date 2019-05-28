package eu.cong.easympc

import java.security.SecureRandom

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import scala.util.Random

class EncoderSpec extends FlatSpec with Checkers {
  import Encoder._
  import ArbitraryHelper._
  implicit val r: Random = new Random(new SecureRandom())

  "big int" should "encode and decode correctly" in {
    check((x: BigInt) => {
      val buf = x.encode()
      x.decode(buf).map(_ == x).get
    })
  }

  it should "encode and decode infinity correctly" in {
    val inf = Group.bigIntGroup.inf
    val buf = inf.encode()
    assert(inf == bigInt.decode(buf).get)
  }

  "curve25519 point" should "encode and decode correctly" in {
    check((p: Group.TPt) => {
      val buf = p.encode()
      p.decode(buf).map(_ == p).get
    })
  }

  it should "encode and decode infinity correctly" in {
    val inf = Group.curve25519Group.inf
    val buf = inf.encode()
    assert(inf == curve25519Point.decode(buf).get)
  }

  it should "fail to decode invalid points" in {
    // TODO
  }

  "curve25519 scalar" should "encode and decode correctly" in {
    check((p: Group.TFe) => {
      val buf = p.encode()
      p.decode(buf).map(_ == p).get
    })
  }
}
