package eu.cong.easympc.protocol

import akka.actor.testkit.typed.scaladsl.ActorTestKit
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}
import WireTransfer._
import eu.cong.easympc.protocol.BankAccount.{Balance, GetBalance}

class WireTransferSpec() extends WordSpecLike with Matchers with BeforeAndAfterAll {
  private val testKit = ActorTestKit()
  private val alice = testKit.spawn(BankAccount(10), "alice")
  private val bob = testKit.spawn(BankAccount(20), "bob")
  private val accountProbe = testKit.createTestProbe[Resp]()
  private val balanceProbe = testKit.createTestProbe[Balance]()

  override def afterAll(): Unit = testKit.shutdownTestKit()

  "a WireTransfer actor" must {
    "succeed when the sender and receiver are working" in {
      testKit.spawn(WireTransfer(accountProbe.ref, bob, alice, 10))
      accountProbe.expectMessage(Done)

      alice ! GetBalance(balanceProbe.ref)
      balanceProbe.expectMessage(Balance(20))

      bob ! GetBalance(balanceProbe.ref)
      balanceProbe.expectMessage(Balance(10))
    }

    "fail when the balance is too low" in {
      testKit.spawn(WireTransfer(accountProbe.ref, bob, alice, 11))
      accountProbe.expectMessage(Failed)
    }

    "fail when a transfer is made between the same user" in {
      testKit.spawn(WireTransfer(accountProbe.ref, alice, alice, 1))
      accountProbe.expectMessage(Failed)
    }
  }
}
