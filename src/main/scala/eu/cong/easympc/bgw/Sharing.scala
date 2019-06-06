package eu.cong.easympc.bgw

import akka.actor.typed.scaladsl._
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.SecretSharing.{XYShare, share}
import eu.cong.easympc.Group

import scala.util.Random

object Sharing {
  def apply[S](implicit g: Group[_, S], r: Random): Behavior[ShareMsg[S]] =
    Behaviors.setup(ctx => new Sharing[S](ctx))

  sealed trait ShareMsg[S]
  final case class Start[S](secret: S, t: Int, n: Int, parent: ActorRef[Shares[S]])
      extends ShareMsg[S]
  final case class Shares[S](shares: Seq[XYShare[S]])
}

/** The [[Sharing]] actor performs secret sharing operations. Consider using multiple
  * actors when computing shares for many secrets concurrently.
  *
  * @param ctx is the actor context.
  */
class Sharing[S](ctx: ActorContext[Sharing.ShareMsg[S]])(implicit g: Group[_, S], r: Random)
    extends AbstractBehavior[Sharing.ShareMsg[S]] {

  import Sharing._

  override def onMessage(msg: ShareMsg[S]): Behavior[ShareMsg[S]] =
    Behaviors.receiveMessage {
      case Start(secret, t, n, parent) =>
        require(n > t)
        parent ! Shares(share(secret, t, n))
        Behaviors.same
    }

  // TODO manage termination?
}
