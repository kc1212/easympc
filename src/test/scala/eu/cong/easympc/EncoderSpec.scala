package eu.cong.easympc

import java.security.SecureRandom

import org.bouncycastle.math.ec.ECPoint
import org.scalatest.FlatSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class EncoderSpec extends FlatSpec with ScalaCheckDrivenPropertyChecks {
  import Encoder._
  implicit val r: Random = new Random(new SecureRandom())

  "big int point" should "encode and decode correctly" in {
    import AbGroupPoint.bigIntGroup
    forAll(ArbitraryHelper.arbPoint.arbitrary) { x =>
      {
        val buf = x.encode()
        x.decode(buf).map(_ == x).get
      }
    }
  }

  "curve25519 point" should "encode and decode correctly" in {
    import AbGroupPoint.curve25519Point
    import ArbitraryHelper.arbPoint
    forAll((p: ECPoint) => {
      val buf = p.encode()
      p.decode(buf).map(_ == p).get
    })
  }

  it should "encode and decode empty correctly" in {
    val grp = AbGroupPoint.curve25519Point
    val buf = grp.empty.encode()
    grp.empty == curve25519Point.decode(buf).get
  }

  it should "fail to decode invalid points" in {
    // TODO
  }
}
