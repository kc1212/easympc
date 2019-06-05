package eu.cong.easympc.bgw

import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, Behavior}

object Controller {
  def apply(myID: Int): Behavior[Command] =
    Behaviors.setup { ctx =>
      new Controller(ctx, myID)
    }

  sealed trait Command
  final case class Exec(expr: Expr, controllers: List[ActorRef[Command]]) extends Command
  final case class AdaptedShares(id: Int, shares: Sharing.Shares) extends Command
}

/** This is the guardian actor for the BGW protocol. One [[Controller]]
  * represents one party, all communication between parties go through
  * the controller. It is also the actor that spawns and manages other
  * actors such as the sharing actor. However, the sharing actors do
  * not communicate between different parties directly, everything is
  * proxied through the controller. We use the pull based approach to
  * implement the communication between controllers. Every controller is
  * responsible for collecting all the data that it needs to do its
  * computation. We assume all controllers are ready to receive messages
  * before any commands are sent.
  */
class Controller(ctx: ActorContext[Controller.Command], myID: Int)
    extends AbstractBehavior[Controller.Command] {

  import Controller._

  override def onMessage(msg: Controller.Command): Behavior[Controller.Command] =
    waitStart

  private val buffer = StashBuffer[Command](capacity = 100)

  private def waitStart: Behavior[Command] = {
    Behaviors.receiveMessage {
      case Exec(expr, allControllers) =>
        // TODO do something with expr
        val sharingActor = ctx.spawn(Sharing(), "sharing-child")
        val secret = Seq() // TODO generate the secret, or more secrets
        sharingActor ! Sharing.Start(secret, ctx.messageAdapter(msg => AdaptedShares(myID, msg)))
        buffer.unstashAll(ctx, compute(allControllers))
      case other =>
        buffer.stash(other)
        Behaviors.same
    }
  }

  private def compute(controllers: List[ActorRef[Command]]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case AdaptedShares(id, share) =>
        ???
      case _ =>
        Behaviors.unhandled
    }
  }
}
