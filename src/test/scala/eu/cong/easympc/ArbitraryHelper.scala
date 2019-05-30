package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Arbitrary

import scala.util.Random

object ArbitraryHelper {
  private val r: Random = new Random(new SecureRandom())

  implicit def arbScalar[S](implicit g: Group[_, S]): Arbitrary[S] = Arbitrary {
    Arbitrary.arbitrary[Int].map { _ =>
      // TODO add generation of the smallest value and largest value
      // ignore the integer and replace it with a random element
      g.rand(r)
    }
  }

  implicit def arbPoint[P, S](implicit g: Group[P, S]): Arbitrary[P] = Arbitrary {
    import Group.PointOps
    arbScalar.arbitrary.map { g.base ** _ }
  }

}
