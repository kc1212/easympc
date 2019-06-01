package eu.cong.easympc.protocol

import akka.actor.{Actor, ActorLogging, ActorRef, Props}

object WireTransfer {
  val prop: Props = Props[WireTransfer]
  case class Transfer(from: ActorRef, to: ActorRef, amount: Int)
  case object Done
  case object Failed
}

class WireTransfer extends Actor {
  import WireTransfer._
  override def receive: Receive = {
    case Transfer(from, to, amount) =>
      if (from.equals(to)) {
        sender ! Failed
        context.stop(self)
      } else {
        from ! BankAccount.Withdraw(amount)
        context.become(awaitWithdraw(to, amount, sender))
      }
  }

  def awaitWithdraw(to: ActorRef, amount: Int, client: ActorRef): Receive = {
    case BankAccount.Done =>
      to ! BankAccount.Deposit(amount)
      context.become(awaitDeposit(client))
    case BankAccount.Failed =>
      client ! Failed
      context.stop(self)
  }

  def awaitDeposit(client: ActorRef): Receive = {
    case BankAccount.Done =>
      client ! Done
      context.stop(self)
  }

}
