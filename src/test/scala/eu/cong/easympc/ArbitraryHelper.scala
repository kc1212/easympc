package eu.cong.easympc

import java.security.SecureRandom

import org.scalacheck.Arbitrary

import scala.util.Random

object ArbitraryHelper {
  private val r: Random = new Random(new SecureRandom())

  implicit def arbScalar(implicit g: Group[_, Group.TFe]): Arbitrary[Group.TFe] = Arbitrary {
    Arbitrary.arbitrary[Int].map { _ =>
      // TODO add generation of the smallest value and largest value
      // ignore the integer and replace it with a random element
      g.rand(r)
    }
  }

  implicit def arbPoint(implicit g: Group[Group.TPt, Group.TFe]): Arbitrary[Group.TPt] = Arbitrary {
    import Group.PointOps
    arbScalar.arbitrary.map { g.base ** _ }
  }

}
