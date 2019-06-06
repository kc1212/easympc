package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers}
import org.scalatest.prop.PropertyChecks

import scala.util.Random
import Group.rand

class SecretSharingSpec extends FlatSpec with PropertyChecks with Matchers {
  implicit private val r: Random = new Random(new SecureRandom())
  private def pairGen(max: Int) =
    for {
      x <- Gen.choose(2, max)
      y <- Gen.choose(2, max)
    } yield (x, y)

  "int group" should "correctly share secrets in a small group" in {
    val p = 23
    implicit val smallGroup: Group[BigInt, BigInt] = Group.customBigIntGroup(2, p)
    forAll(pairGen(p)) {
      case (t: Int, n: Int) =>
        whenever(t > 2 && n <= p && t <= n) {
          val secret = rand()
          val shares = SecretSharing.share(secret, t, n)
          shares.size should be(n)
          SecretSharing.combine(shares) should be(secret)
        }
    }
  }

  it should "correctly share secrets in a large group" in {
    val maxShares = 66
    implicit val g: Group[BigInt, BigInt] = Group.bigIntGroup
    forAll(pairGen(maxShares)) {
      case (t: Int, n: Int) =>
        whenever(t > 2 && n <= maxShares && t <= n) {
          val secret = rand()
          val shares = SecretSharing.share(secret, t, n)
          shares.size should be(n)
          SecretSharing.combine(shares) should be(secret)
        }
    }
  }
}
