package eu.cong.easympc.bgw

import akka.actor.typed.scaladsl._
import akka.actor.typed.{ActorRef, Behavior}

object Sharing {
  def apply(): Behavior[ShareMsg] =
    Behaviors.setup(ctx => new Sharing(ctx))

  sealed trait ShareMsg
  final case class Start(secret: Seq[Byte], parent: ActorRef[Shares]) extends ShareMsg
  final case class Shares(secret: Seq[Byte], shares: Seq[Seq[Byte]])
}

/** The [[Sharing]] actor performs secret sharing operations. Consider using multiple
  * actors when computing shares for many secrets concurrently.
  *
  * @param ctx is the actor context.
  */
class Sharing(ctx: ActorContext[Sharing.ShareMsg]) extends AbstractBehavior[Sharing.ShareMsg] {
  import Sharing._
  override def onMessage(msg: ShareMsg): Behavior[ShareMsg] =
    Behaviors.receiveMessage {
      case Start(secret, parent) =>
        // TODO compute the shares
        val share = Shares(secret, Seq())
        parent ! share
        Behaviors.same
    }
}
