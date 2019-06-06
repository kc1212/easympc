package eu.cong.easympc.bgw

import akka.actor.Cancellable
import akka.actor.typed.scaladsl.{AbstractBehavior, ActorContext, Behaviors, StashBuffer}
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.SecretSharing.XYShare
import eu.cong.easympc.{Expr, Group}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.util.Random

object Controller {
  def apply[S](implicit g: Group[_, S], r: Random): Behavior[Command[S]] =
    Behaviors.setup { ctx =>
      new Controller(ctx)
    }

  private type AR[S] = ActorRef[Command[S]]

  sealed trait Command[S]

  final case class Exec[S](expr: Expr[AR[S], S], input: S, controllers: Set[AR[S]]) extends Command[S]

  // These messages are for exchanging shares between controllers.
  // TODO add request ID
  final case class GetShares[S](replyTo: AR[S]) extends Command[S]
  final case class Shares[S](from: AR[S], share: XYShare[S]) extends Command[S]

  // These messages are for getting the final result.
  // TODO add request ID
  final case class GetResult[S](replyTo: ActorRef[Result[S]]) extends Command[S]
  final case class Result[S](inner: Map[AR[S], XYShare[S]])

  // The adapter is only internal communication.
  private final case class AdaptedShares[S](shares: Sharing.Shares[S]) extends Command[S]
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
class Controller[S](ctx: ActorContext[Controller.Command[S]])(implicit g: Group[_, S], r: Random)
    extends AbstractBehavior[Controller.Command[S]] {

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
  private class MainState(val expr: Expr[AR[S], S],
                          val input: S,
                          val controllers: Set[AR[S]],
                          val myShares: Map[AR[S], XYShare[S]],
                          var otherShares: Map[AR[S], XYShare[S]]) {
    val validIDs: Set[AR[S]] = Expr.GetVars(expr).map(_.x).toSet
    def finished(): Boolean = validIDs subsetOf otherShares.keySet
  }

  implicit val ec: ExecutionContext = ctx.executionContext
  private val log = ctx.log

  override def onMessage(msg: Controller.Command[S]): Behavior[Controller.Command[S]] =
    waitStart

  private val buffer = StashBuffer[Command[S]](capacity = 100)

  private def waitStart: Behavior[Command[S]] = {
    Behaviors.receiveMessage {
      case Exec(expr, input, allControllers) =>
        val n = allControllers.size
        require(n > 2)

        // Create a child actor for creating the shares because it is slow on large input.
        val sharingActor = ctx.spawn(Sharing(g, r), "sharing-child")

        // Start our own secret sharing process with out own input.
        // The default threshold is n-1 out of n, we may need to revisit it later.
        sharingActor ! Sharing.Start(input, n - 1, n, ctx.messageAdapter(AdaptedShares(_)))
        waitMyShares(expr, input, allControllers)
      case other =>
        buffer.stash(other)
        Behaviors.same
    }
  }

  private def waitMyShares(expr: Expr[AR[S], S], input: S, controllers: Set[AR[S]]): Behavior[Command[S]] = {
    Behaviors.receiveMessage {
      case AdaptedShares(shares) =>
        require(shares.shares.size == controllers.size)

        // Get shares from the other controllers periodically.
        val cancellables = controllers.map { controller =>
          (controller, ctx.system.scheduler.schedule(0.second, 500.milliseconds)(controller ! GetShares(ctx.self)))
        }

        val mappedShares = controllers zip shares.shares

        val state = new MainState(expr, input, controllers, mappedShares.toMap, Map.empty)
        buffer.unstashAll(ctx, compute(state, cancellables.toMap))
      case other =>
        buffer.stash(other)
        Behaviors.same
    }
  }

  private def compute(state: MainState, cancellable: Map[AR[S], Cancellable]): Behavior[Command[S]] = {
    Behaviors.receiveMessage {
      case GetShares(replyTo) =>
        state.myShares.get(replyTo) match {
          case Some(s) =>
            replyTo ! Shares(ctx.self, s)
            Behaviors.same
          case _ =>
            log.warning("share does not exist")
            Behaviors.same
        }
      case Shares(from, share) =>
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
