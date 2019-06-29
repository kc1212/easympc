package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class SecretSharingSpec extends FlatSpec with ScalaCheckDrivenPropertyChecks with Matchers {
  implicit private val r: Random = new Random(new SecureRandom())
  private def pairGen(max: Int) =
    for {
      x <- Gen.choose(2, max)
      y <- Gen.choose(2, max)
    } yield (x, y)

  "int group" should "correctly share secrets in a small group" in {
    val p = 23
    implicit val smallGroup: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(p)
    forAll(pairGen(p)) {
      case (t: Int, n: Int) =>
        whenever(t > 2 && n <= p && t <= n) {
          val secret = smallGroup.randElem()
          val shares = SecretSharing.share(secret, t, n)
          shares.size should be(n)
          SecretSharing.combine(shares) should be(secret)
        }
    }
  }

  /*
  it should "correctly share secrets in a large group" in {
    val maxShares = 66
    implicit val grp: AbGroupScalar = AbGroupScalar.curve25519Scalar
    forAll(pairGen(maxShares)) {
      case (t: Int, n: Int) =>
        whenever(t > 2 && n <= maxShares && t <= n) {
          val secret = grp.randElem()
          val shares = SecretSharing.share(secret, t, n)
          shares.size should be(n)
          SecretSharing.combine(shares) should be(secret)
        }
    }
  }
 */
}
