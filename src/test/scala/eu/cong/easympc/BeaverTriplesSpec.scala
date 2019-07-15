package eu.cong.easympc

import org.scalatest.{Matchers, WordSpec}
import java.security.SecureRandom

import eu.cong.easympc.AbGroupScalar.Ops
import eu.cong.easympc.SecretSharing.Share
import org.scalacheck.Gen
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

import scala.util.Random

class BeaverTriplesSpec extends WordSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  private def thresholdGen(max: Int) =
    for {
      t <- Gen.choose(2, max)
      n <- Gen.choose(2, max) suchThat (_ >= t)
      a <- ArbitraryHelper.arbScalar.arbitrary
      b <- ArbitraryHelper.arbScalar.arbitrary
      x <- ArbitraryHelper.arbScalar.arbitrary
      y <- ArbitraryHelper.arbScalar.arbitrary
    } yield (a, b, x, y, t, n)

  private def endToEndTest(a: BigInt, b: BigInt, x: BigInt, y: BigInt, t: Int, n: Int)(implicit grp: AbGroupScalar, r: Random) = {
    val (a_box, b_box, c_box) = BeaverTriples(a, b, t, n).head

    // check that beaver triples are generated correctly
    SecretSharing.combine(c_box) shouldBe (a <*> b)

    val x_box = SecretSharing.share(x, t, n)
    val y_box = SecretSharing.share(y, t, n)

    val (x_a_shares, y_b_shares) = (0 until n).map { i =>
      x_box(i).x shouldBe y_box(i).x
      x_box(i).x shouldBe a_box(i).x
      x_box(i).x shouldBe b_box(i).x

      val x_a_box = x_box(i).y <-> a_box(i).y
      val y_b_box = y_box(i).y <-> b_box(i).y
      (Share(x_box(i).x, x_a_box), Share(y_box(i).x, y_b_box))
    }.unzip

    // check that x-a and y-b are correctly computed
    val x_a = SecretSharing.combine(x_a_shares)
    val y_b = SecretSharing.combine(y_b_shares)
    x_a shouldBe (x <-> a)
    y_b shouldBe (y <-> b)

    // check that the final value z=x*y is correct
    val z_box = (0 until n).map { i =>
      c_box(i).x shouldBe x_box(i).x
      c_box(i).x shouldBe y_box(i).x

      val z_box = c_box(i).y <+> (x_box(i).y <*> y_b) <+> (y_box(i).y <*> x_a) <-> (x_a <*> y_b)
      Share(c_box(i).x, z_box)
    }
    val z = SecretSharing.combine(z_box)
    z shouldBe (x <*> y)
  }

  "beaver triples" should {
    "work in a small group" in {
      implicit val grp: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(13)
      implicit val r: Random = new Random(new SecureRandom())
      forAll(thresholdGen(7)) {
        case (a, b, x, y, t, n) => endToEndTest(a, b, x, y, t, n)
      }
    }

    "work in a large group" in {
      implicit val grp: AbGroupScalar = AbGroupScalar.curve25519Scalar
      implicit val r: Random = new Random(new SecureRandom())
      forAll(thresholdGen(7)) {
        case (a, b, x, y, t, n) => endToEndTest(a, b, x, y, t, n)
      }
    }
  }
}
