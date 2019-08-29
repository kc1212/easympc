package eu.cong.easympc.protocol

import akka.actor.testkit.typed.scaladsl.{BehaviorTestKit, TestInbox}
import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class BulletinBoardSpec extends WordSpec with ScalaCheckDrivenPropertyChecks {
  import BulletinBoard._
  "a bulletin board actor" must {
    "remember records" in {
      forAll { (k1: Int, k2: Int, v: BigInt) =>
        whenever(k1 != k2) {
          val kit = BehaviorTestKit(behavior())
          val inbox = TestInbox[Msg]()

          kit.run(Set(inbox.ref, k1, v))
          inbox.expectMessage(Ok(k1))

          kit.run(Get(inbox.ref, k1))
          inbox.expectMessage(Val(v))

          // The behaviour should be silent if the value is not set
          // and then reply when it is.
          kit.run(Get(inbox.ref, k2))
          assert(!inbox.hasMessages)

          kit.run(Set(inbox.ref, k2, v))
          inbox.expectMessage(Ok(k2))
          inbox.expectMessage(Val(v))
        }
      }
    }

    "fail when the key already exists" in {
      forAll { (k: Int, v: BigInt) =>
        val kit = BehaviorTestKit(behavior())
        val inbox = TestInbox[Msg]()

        kit.run(Set(inbox.ref, k, v))
        inbox.expectMessage(Ok(k))

        kit.run(Set(inbox.ref, k, v))
        inbox.expectMessage(Fail(k))
      }
    }
  }
}
