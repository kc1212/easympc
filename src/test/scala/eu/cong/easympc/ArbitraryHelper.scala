package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Arbitrary

import scala.util.Random

object ArbitraryHelper {
  private val r: Random = new Random(new SecureRandom())

  implicit def arbScalar(implicit grp: AbGroupScalar): Arbitrary[BigInt] = Arbitrary {
    Arbitrary.arbitrary[Int].map { _ =>
      // TODO add generation of the smallest value and largest value
      // ignore the integer and replace it with a random element
      grp.randElem()(r)
    }
  }

  implicit def arbPoint[P](implicit grp: AbGroupPoint[P]): Arbitrary[P] = Arbitrary {
    import AbGroupPoint.Ops
    arbScalar.arbitrary.map { grp.base |*| _ }
  }
}
