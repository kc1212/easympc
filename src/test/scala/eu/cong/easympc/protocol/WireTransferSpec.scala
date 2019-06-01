package eu.cong.easympc.protocol

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import WireTransfer._
import eu.cong.easympc.protocol.BankAccount.{Balance, GetBalance}

class WireTransferSpec() extends WordSpecLike with Matchers with BeforeAndAfterAll {
  val testKit = ActorTestKit()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "a WireTransfer actor" must {
    "succeed when the sender and receiver are working" in {
      val alice = testKit.spawn(BankAccount(10), "alice")
      val bob = testKit.spawn(BankAccount(20), "bob")
      val probe = testKit.createTestProbe[Resp]()

      testKit.spawn(WireTransfer(probe.ref, alice, bob, 10))
      probe.expectMessage(Done)

      val balanceProbe = testKit.createTestProbe[Balance]()
      alice ! GetBalance(balanceProbe.ref)
      balanceProbe.expectMessage(Balance(0))

      bob ! GetBalance(balanceProbe.ref)
      balanceProbe.expectMessage(Balance(30))
    }

    "fail when the balance is too low" in {
      val alice = testKit.spawn(BankAccount(10), "alice")
      val bob = testKit.spawn(BankAccount(20), "bob")
      val probe = testKit.createTestProbe[Resp]()

      testKit.spawn(WireTransfer(probe.ref, alice, bob, 11))
      probe.expectMessage(Failed)
    }

    "fail when a transfer is made between the same user" in {
      val alice = testKit.spawn(BankAccount(10), "alice")
      val probe = testKit.createTestProbe[Resp]()

      testKit.spawn(WireTransfer(probe.ref, alice, alice, 1))
      probe.expectMessage(Failed)
    }
  }
}
