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

  private def workloadGen(max: Int)(implicit grp: AbGroupScalar) = {
    for {
      n <- Gen.choose(2, max)
      xs <- Gen.listOfN(n, ArbitraryHelper.arbScalar.arbitrary)
      d <- Gen.choose(0, 4)
    } yield (xs.size + d, xs)
  }

  "small int group" should "correctly share secrets" in {
    val p = 23
    implicit val smallGroup: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(p)
    forAll(workloadGen(7)) {
      case (n, xs) =>
        val secret = xs(0)
        val shares = SecretSharing.evalAll(n, xs)
        shares.size should be(n)
        SecretSharing.combine(shares) should be(secret)
    }
  }

  "large int group" should "correctly share secrets" in {
    implicit val grp: AbGroupScalar = AbGroupScalar.curve25519Scalar
    forAll(workloadGen(7)) {
      case (n, xs) =>
        val secret = xs(0)
        val shares = SecretSharing.evalAll(n, xs)
        shares.size should be(n)
        SecretSharing.combine(shares) should be(secret)
    }
  }

  private def thresholdGen(max: Int) =
    for {
      t <- Gen.choose(2, max)
      n <- Gen.choose(2, max) suchThat (_ >= t)
      secret <- ArbitraryHelper.arbScalar.arbitrary
    } yield (secret, t, n)

  it should "work with randomized coefficients" in {
    implicit val grp: AbGroupScalar = AbGroupScalar.curve25519Scalar
    forAll(thresholdGen(7)) {
      case (secret, t, n) =>
        val shares = SecretSharing.share(secret, t, n)
        SecretSharing.combine(shares) should be(secret)
    }
  }
}
