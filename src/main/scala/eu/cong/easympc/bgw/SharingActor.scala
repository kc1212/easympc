package eu.cong.easympc.bgw

import akka.actor.typed.scaladsl._
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.SecretSharing.{XYShare, share}
import eu.cong.easympc.Group

import scala.util.Random

object SharingActor {
  def apply[S]()(implicit g: Group[_, S], r: Random): Behavior[SharingActorMsg[S]] =
    Behaviors.setup(ctx => new SharingActor[S](ctx))

  sealed trait SharingActorMsg[S]
  final case class Start[S](secret: S, t: Int, n: Int, parent: ActorRef[Shares[S]])(implicit g: Group[_, S])
      extends SharingActorMsg[S]
  final case class Shares[S](shares: Seq[XYShare[S]])(implicit g: Group[_, S])

}

/** The [[SharingActor]] actor performs secret sharing operations.
  * Consider using multiple actors when computing shares for many secrets concurrently.
  *
  * @param ctx is the actor context.
  */
class SharingActor[S](ctx: ActorContext[SharingActor.SharingActorMsg[S]])(implicit g: Group[_, S], r: Random)
    extends AbstractBehavior[SharingActor.SharingActorMsg[S]] {

  import SharingActor._

  override def onMessage(msg: SharingActorMsg[S]): Behavior[SharingActorMsg[S]] = {
    msg match {
      case Start(secret, t, n, parent) =>
        require(n > t)
        parent ! Shares(share(secret, t, n))
        Behaviors.same
      case _ =>
        Behavior.unhandled
    }
  }

  // TODO manage termination?
}
