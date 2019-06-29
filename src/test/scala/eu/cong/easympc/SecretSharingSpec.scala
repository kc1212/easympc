package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Gen
import org.scalatest.{FlatSpec, Matchers, PrivateMethodTester}
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class SecretSharingSpec extends FlatSpec with ScalaCheckDrivenPropertyChecks with Matchers with PrivateMethodTester {
  implicit private val r: Random = new Random(new SecureRandom())

  "polynomail evaluation" should "work on an example" in {
    // f(x) = 2 + 7x + 12x^2 mod 13
    // f(2) = 12
    // f(3) = 1
    // f(9) = 10
    val p = 13
    implicit val smallGroup: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(p)
    val coeffs = Seq(2, 7, 12).map(BigInt(_))
    SecretSharing.eval(BigInt(2), coeffs) should be(BigInt(12))
    SecretSharing.eval(BigInt(3), coeffs) should be(BigInt(1))
    SecretSharing.eval(BigInt(9), coeffs) should be(BigInt(10))
  }

  "lagrange basis" should "work on an example" in {
    // we evaluate on the points (x0, x1, x2, x3) = (1, 2, 3, 4)
    // lagrange_basis(j) = product[ -x_m / (x_j-x_m) ] mod 13
    // for m = 0 to 3 where m != j
    // lagrange_basis(0) = 4
    // lagrange_basis(1) = 7
    // lagrange_basis(2) = 4
    // lagrange_basis(3) = 12
    val p = 13
    implicit val smallGroup: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(p)
    val xs = Seq(1, 2, 3, 4).map(BigInt(_))
    SecretSharing.lagrange_basis(0)(0, xs) should be(BigInt(4))
    SecretSharing.lagrange_basis(1)(0, xs) should be(BigInt(7))
    SecretSharing.lagrange_basis(2)(0, xs) should be(BigInt(4))
    SecretSharing.lagrange_basis(3)(0, xs) should be(BigInt(12))
  }

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
