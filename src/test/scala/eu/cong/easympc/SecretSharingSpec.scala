package eu.cong.easympc

import java.security.SecureRandom

import org.scalatest.FlatSpec

import scala.util.Random

class SecretSharingSpec extends FlatSpec {
  implicit val r: Random = new Random(new SecureRandom())

  "small int group" should "correctly share secrets" in {
    implicit val smallGroup: Group[BigInt, BigInt] = Group.customBigIntGroup(2, 13)
    val secret: BigInt = 2
    val n = 4
    val shares = SecretSharing.share(Some(secret), 3, n)
    assert(shares.size == n)
    assert(SecretSharing.combine(shares) == secret)
  }
}
