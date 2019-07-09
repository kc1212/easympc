package eu.cong.easympc

import java.security.SecureRandom

import eu.cong.easympc.SecretSharing.Share
import org.scalatest.{Matchers, WordSpec}

import scala.util.Random

class BGWSpec extends WordSpec with Matchers {

  implicit private val r: Random = new Random(new SecureRandom())
  import eu.cong.easympc.StackMachine._

  "simple addition using small integer group" should {
    "succeed" in {
      val p = 13
      implicit val smallGroup: AbGroupScalar = AbGroupScalar.additiveGroupFromOrder(p)

      val id0 = 0
      val id1 = 1
      val roster = Set(id0, id1)
      val prog = List(Push(id0), Push(id1), Add())

      val secret0 = BigInt(4)
      val secret1 = BigInt(5)

      val share0 = SecretSharing.share(secret0, 2, 2)
      val share1 = SecretSharing.share(secret1, 2, 2)

      val res0 = BGW(id0, roster, prog, Map(0 -> share0(0), 1 -> share1(0)), List())
      val res1 = BGW(id1, roster, prog, Map(0 -> share0(1), 1 -> share1(1)), List())

      SecretSharing.combine(List(res0.get, res1.get)) should equal(secret0 + secret1)
    }
  }

  def keyShares[T](shares: Seq[Share], ids: Set[T]): Map[T, Share] = {
    require(shares.size == ids.size)
    ids.zip(shares).toMap
  }
}
