package eu.cong.easympc.bgw

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.SecretSharing.XYShare
import eu.cong.easympc.{AbGroupSuite, Expr}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

object Controller {
  def apply[P](implicit grp: AbGroupSuite[P], r: Random): Behavior[Command] =
    Behaviors.setup { ctx =>
      new Controller(ctx)
    }

  private type AR = ActorRef[Command]

  sealed trait Command

  final case class Exec(expr: Expr[AR, BigInt], input: BigInt, controllers: Set[AR]) extends Command

  // These messages are for exchanging shares between controllers.
  // TODO add request ID
  final case class GetOneShare(replyTo: AR) extends Command
  final case class OneShares(from: AR, share: XYShare) extends Command

  // These messages are for getting the final result.
  // TODO add request ID
  final case class GetResult(replyTo: ActorRef[Result]) extends Command
  final case class Result(inner: Map[AR, XYShare])

  // The adapter is only internal communication.
  private final case class AdaptedShares(shares: SharingActor.Shares) extends Command
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
class Controller[P](ctx: ActorContext[Controller.Command])(implicit grp: AbGroupSuite[P], r: Random)
    extends AbstractBehavior[Controller.Command] {

  import Controller._

  /** This class keeps the internal state of the BGW protocol.
    *
    * @param expr is the expression to evaluate
    * @param input holds the secret data for this actor
    * @param controllers is a list of all the controllers
    * @param myShares is a mutable map for the shares created by this
    *                 node
    * @param otherShares is a mutable map, the key is the var ID, the
    *                 value is the share from others
    */
  private class MainState(val expr: Expr[AR, BigInt],
                          val input: BigInt,
                          val controllers: Set[AR],
                          val myShares: Map[AR, XYShare],
                          var otherShares: Map[AR, XYShare]) {
    val validIDs: Set[AR] = Expr.GetVars(expr).map(_.x).toSet
    def finished(): Boolean = validIDs subsetOf otherShares.keySet
  }

  implicit val ec: ExecutionContext = ctx.executionContext
  private val log = ctx.log

  override def onMessage(msg: Controller.Command): Behavior[Controller.Command] =
    waitStart

  private val buffer = StashBuffer[Command](capacity = 100)

  private def waitStart: Behavior[Command] = {
    Behaviors.receiveMessage {
      case Exec(expr, input, allControllers) =>
        val n = allControllers.size
        require(n > 2)

        // Create a child actor for creating the shares because it is slow on large input.
        val sharingActor = ctx.spawn(SharingActor(), "sharing-child")

        // Start our own secret sharing process with out own input.
        // The default threshold is n-1 out of n, we may need to revisit it later.
        sharingActor ! SharingActor.Start(input, n - 1, n, ctx.messageAdapter(AdaptedShares))
        waitMyShares(expr, input, allControllers)
      case other =>
        buffer.stash(other)
        Behaviors.same
    }
  }

  private def waitMyShares(expr: Expr[AR, BigInt], input: BigInt, controllers: Set[AR]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case AdaptedShares(shares) =>
        require(shares.shares.size == controllers.size)

        // Get shares from the other controllers periodically.
        val cancellables = controllers.map { controller =>
          (controller, ctx.system.scheduler.schedule(0.second, 500.milliseconds)(controller ! GetOneShare(ctx.self)))
        }

        val mappedShares = controllers zip shares.shares

        val state = new MainState(expr, input, controllers, mappedShares.toMap, Map.empty)
        buffer.unstashAll(ctx, compute(state, cancellables.toMap))
      case other =>
        buffer.stash(other)
        Behaviors.same
    }
  }

  private def compute(state: MainState, cancellable: Map[AR, Cancellable]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case GetOneShare(replyTo) =>
        state.myShares.get(replyTo) match {
          case Some(s) =>
            replyTo ! OneShares(ctx.self, s)
            Behaviors.same
          case _ =>
            log.warning("share does not exist")
            Behaviors.same
        }
      case OneShares(from, share) =>
        if (!state.validIDs.contains(from)) {
          log.warning("share for var {} is not in the accepted set", from)
          Behavior.same
        } else {
          if (state.otherShares.keySet contains from) {
            log.warning("already got a share for var {}", from)
            Behavior.same
          } else {
            state.otherShares += (from -> share)
            cancellable(from).cancel()
            Behavior.same
          }
        }
      case GetResult(replyTo) =>
        if (state.finished()) {
          replyTo ! Result(state.otherShares)
        } else {
          replyTo ! Result(Map.empty)
        }
        Behavior.same
      case _ =>
        Behaviors.unhandled
    }
  }

}
