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
    val (a_boxes, b_boxes, c_boxes) = BeaverTriples(a, b, t, n).head

    // check that beaver triples are generated correctly
    SecretSharing.combine(c_boxes) shouldBe (a <*> b)

    val x_boxes = SecretSharing.share(x, t, n)
    val y_boxes = SecretSharing.share(y, t, n)

    val (x_a_boxes, y_b_boxes) = (0 until n).map { i =>
      x_boxes(i).x shouldBe y_boxes(i).x
      x_boxes(i).x shouldBe a_boxes(i).x
      x_boxes(i).x shouldBe b_boxes(i).x

      val x_a_box = x_boxes(i).y <-> a_boxes(i).y
      val y_b_box = y_boxes(i).y <-> b_boxes(i).y
      (Share(x_boxes(i).x, x_a_box), Share(y_boxes(i).x, y_b_box))
    }.unzip

    // check that x-a and y-b are correctly computed
    val x_a = SecretSharing.combine(x_a_boxes)
    val y_b = SecretSharing.combine(y_b_boxes)
    x_a shouldBe (x <-> a)
    y_b shouldBe (y <-> b)

    // check that the final value z=x*y is correct
    val z_boxes = (0 until n).map { i =>
      c_boxes(i).x shouldBe x_boxes(i).x
      c_boxes(i).x shouldBe y_boxes(i).x

      val z_box = c_boxes(i).y <+> (x_boxes(i).y <*> y_b) <+> (y_boxes(i).y <*> x_a) <-> (x_a <*> y_b)
      Share(c_boxes(i).x, z_box)
    }
    val z = SecretSharing.combine(z_boxes)
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
