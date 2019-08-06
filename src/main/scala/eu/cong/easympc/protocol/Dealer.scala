package eu.cong.easympc.protocol

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{ActorContext, Behaviors}
import eu.cong.easympc.{AbGroupScalar, BeaverTriples}
import eu.cong.easympc.SecretSharing.Share

import scala.util.Random

object Dealer {
  sealed trait Msg
  final case class GetTriple(replyTo: ActorRef[Triple], tripleIndex: Int, senderIndex: Int) extends Msg
  final case class Triple(a: Share, b: Share, c: Share) extends Msg

  private def getTriple(tripleIndex: Int, senderIndex: Int, triples: LazyList[(Seq[Share], Seq[Share], Seq[Share])]): Triple = {
    // TODO find the index of the sender and the index of the triple
    // TODO determine the senderIndex from ActorRef[Triple]
    ???
  }
}

class Dealer(n: Int)(implicit g: AbGroupScalar, r: Random) {
  import Dealer._

  val behavior: Behavior[Msg] = {
    require(n > 2)
    Behaviors.setup { _ =>
      val triples = BeaverTriples((n / 2) + 1, n)
      val inner: Behavior[Msg] =
        Behaviors.receiveMessage {
          case GetTriple(sender, tripleIndex, senderIndex) =>
            sender ! getTriple(tripleIndex, senderIndex, triples)
            Behavior.same
          case _ =>
            Behavior.unhandled
        }
      inner
    }
  }
}
