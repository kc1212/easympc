package eu.cong.easympc.bgw

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import eu.cong.easympc.{AbGroupScalar, SecretSharing}
import eu.cong.easympc.bgw.SharingActor.Start

import scala.util.Random

class SharingActorSpec extends WordSpecLike with Matchers with BeforeAndAfterAll {
  /*
  private val testKit = ActorTestKit()
  implicit val grp: AbGroupScalar = AbGroupScalar.curve25519Scalar
  implicit val r: Random = Random

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "A sharing actor" must {
    "produce the correct shares" in {
      // TODO quickcheck
      val sharingRef = testKit.spawn(SharingActor())
      // TODO write a test probe wrapper that finds the type implicitly
      val sharingProbe = testKit.createTestProbe[SharingActor.Shares]()
      val secret = grp.randElem()
      sharingRef ! Start(secret, 4, 5, sharingProbe.ref)
      val finalShares = sharingProbe.receiveMessage().shares

      SecretSharing.combine(finalShares) should be(secret)
    }
  }
   */
}
