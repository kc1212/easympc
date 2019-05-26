package eu.cong.easympc

import java.security.SecureRandom

import org.scalatest.prop.Checkers
import org.scalatest.FlatSpec

import scala.util.Random

class EncoderSpec extends FlatSpec with Checkers {
  import Encoder._
  implicit val r: Random = new Random(new SecureRandom())

  "big int" should "encode and decode correctly" in {
    check((x: BigInt) => {
      val buf = x.encode()
      x.decode(buf).map(_ == x).get
    })
  }

}
