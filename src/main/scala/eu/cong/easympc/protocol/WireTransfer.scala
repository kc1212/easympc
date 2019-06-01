package eu.cong.easympc.protocol

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.protocol.BankAccount._

object WireTransfer {

  sealed trait Command

  sealed trait Resp extends Command
  final case object Done extends Resp
  final case object Failed extends Resp

  private final case object AdaptedWithdrawn extends Command
  private final case object AdaptedWithdrawFailed extends Command
  private final case object AdaptedDeposited extends Command

  private val withdrawAdapter = (x: WithdrawReply) =>
    x match {
      case Withdrawn      => AdaptedWithdrawn
      case WithdrawFailed => AdaptedWithdrawFailed
  }

  private val depositAdapter = (x: Deposited.type) =>
    x match {
      case Deposited => AdaptedDeposited
  }

  def apply(client: ActorRef[Resp],
            from: ActorRef[Withdraw],
            to: ActorRef[Deposit],
            amount: Long): Behavior[Command] = {

    Behaviors.setup { ctx =>
      if (from.equals(to)) {
        client ! Failed
        Behaviors.stopped
      } else {
        from ! Withdraw(amount, ctx.messageAdapter(withdrawAdapter))
        awaitWithdraw(to, amount, client)
      }
    }
  }

  private def awaitWithdraw(to: ActorRef[Deposit],
                            amount: Long,
                            client: ActorRef[Resp]): Behavior[Command] = {
    Behaviors.receive {
      case (_, AdaptedWithdrawFailed) =>
        client ! Failed
        Behaviors.stopped
      case (ctx, AdaptedWithdrawn) =>
        val adapter = ctx.messageAdapter(depositAdapter)
        to ! Deposit(amount, adapter)
        awaitDeposit(client)
      case _ =>
        client ! Failed
        Behaviors.unhandled
    }
  }

  private def awaitDeposit(client: ActorRef[Resp]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case AdaptedDeposited =>
        client ! Done
        Behaviors.stopped
      case _ =>
        client ! Failed
        Behaviors.unhandled
    }
  }
}
