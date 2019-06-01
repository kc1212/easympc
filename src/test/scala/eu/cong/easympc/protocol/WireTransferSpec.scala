package eu.cong.easympc.protocol

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

import WireTransfer._

class WireTransferSpec()
    extends TestKit(ActorSystem("WireTransferSpec"))
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "a WireTransfer actor" must {
    "succeed when the sender and receiver are working" in {
      val xfer = system.actorOf(WireTransfer.prop)
      val alice = system.actorOf(BankAccount.prop(100))
      val bob = system.actorOf(BankAccount.prop(200))

      xfer ! Transfer(alice, bob, 100)
      expectMsg(Done)
      alice ! BankAccount.Balance
      expectMsg(0)
      bob ! BankAccount.Balance
      expectMsg(300)
    }

    "fail when a transfer is made between the same user" in {
      val xfer = system.actorOf(WireTransfer.prop)
      val alice = system.actorOf(BankAccount.prop(100))

      xfer ! Transfer(alice, alice, 100)
      expectMsg(Failed)
    }

    "fail when balance is too low" in {
      val xfer = system.actorOf(WireTransfer.prop)
      val alice = system.actorOf(BankAccount.prop(10))
      val bob = system.actorOf(BankAccount.prop(0))

      xfer ! Transfer(alice, bob, 11)
      expectMsg(Failed)
    }
  }
}
