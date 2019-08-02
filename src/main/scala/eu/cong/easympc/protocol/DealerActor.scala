package eu.cong.easympc.protocol

import akka.actor.{Actor, ActorRef, Props}
import eu.cong.easympc.SecretSharing.Share

object DealerActor {
  def prop(roster: Set[ActorRef]): Props = Props(new DealerActor(roster))
  case class GetTriple(pc: Int)
  case class Triple(a: Share, b: Share, c: Share)
}

class DealerActor(roster: Set[ActorRef]) extends Actor {
  // TODO pre-generate some triples
  override def receive: Receive = ???
}
