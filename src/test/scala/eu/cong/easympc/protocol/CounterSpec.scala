package eu.cong.easympc.protocol

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

class CounterSpec()
    extends TestKit(ActorSystem("CounterSpec"))
    with ImplicitSender
    with WordSpecLike
    with Matchers
    with BeforeAndAfterAll {

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "a Counter actor" must {
    "send back the value incremented by one" in {
      val counter = system.actorOf(Counter.prop(2))
      counter ! "incr"
      counter ! "get"
      expectMsg(3)
    }
  }
}
