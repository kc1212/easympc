package eu.cong.easympc.protocol

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}
import eu.cong.easympc.protocol.BankAccount._

object WireTransfer {

  sealed trait Command

  sealed trait Resp extends Command
  final case object Done extends Resp
  final case object Failed extends Resp

  private final case class WithdrawReplyAdapter(msg: WithdrawReply) extends Command
  private final case class DepositedAdapter(msg: Deposited.type) extends Command

  def apply(client: ActorRef[Resp],
            from: ActorRef[Withdraw],
            to: ActorRef[Deposit],
            amount: Long): Behavior[Command] = {

    Behaviors.setup { ctx =>
      if (from.equals(to)) {
        client ! Failed
        Behaviors.stopped
      } else {
        val adapter = ctx.messageAdapter(WithdrawReplyAdapter)
        from ! Withdraw(amount, adapter)
        awaitWithdraw(to, amount, client)
      }
    }
  }

  private def awaitWithdraw(to: ActorRef[Deposit],
                            amount: Long,
                            client: ActorRef[Resp]): Behavior[Command] = {
    Behaviors.receive {
      case (ctx, WithdrawReplyAdapter(msg)) =>
        msg match {
          case WithdrawFailed =>
            client ! Failed
            Behaviors.stopped
          case Withdrawn =>
            val adapter = ctx.messageAdapter(DepositedAdapter)
            to ! Deposit(amount, adapter)
            awaitDeposit(client)
        }
      case _ =>
        client ! Failed
        Behaviors.unhandled
    }
  }

  private def awaitDeposit(client: ActorRef[Resp]): Behavior[Command] = {
    Behaviors.receiveMessage {
      case DepositedAdapter(_) =>
        client ! Done
        Behaviors.stopped
      case _ =>
        client ! Failed
        Behaviors.unhandled
    }
  }
}
