package eu.cong.easympc.bgw

import akka.actor.typed.scaladsl._
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.SecretSharing.{XYShare, share}
import eu.cong.easympc.AbGroupScalar

import scala.util.Random

object SharingActor {
  def apply[S]()(implicit g: AbGroupScalar, r: Random): Behavior[SharingActorMsg] =
    Behaviors.setup(ctx => new SharingActor(ctx))

  sealed trait SharingActorMsg
  final case class Start(secret: BigInt, t: Int, n: Int, parent: ActorRef[Shares])(implicit g: AbGroupScalar)
      extends SharingActorMsg
  final case class Shares(shares: Seq[XYShare])(implicit g: AbGroupScalar)

}

/** The [[SharingActor]] actor performs secret sharing operations.
  * Consider using multiple actors when computing shares for many secrets concurrently.
  *
  * @param ctx is the actor context.
  */
class SharingActor(ctx: ActorContext[SharingActor.SharingActorMsg])(implicit g: AbGroupScalar, r: Random)
    extends AbstractBehavior[SharingActor.SharingActorMsg] {

  import SharingActor._

  override def onMessage(msg: SharingActorMsg): Behavior[SharingActorMsg] = {
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
