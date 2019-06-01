package eu.cong.easympc.protocol

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ActorRef, Behavior}

object BankAccount {
  sealed trait Command

  final case class GetBalance(replyTo: ActorRef[Balance]) extends Command
  final case class Balance(balance: Long)

  final case class Deposit(amount: Long, replyTo: ActorRef[Deposited.type]) extends Command
  final case object Deposited

  final case class Withdraw(amount: Long, replyTo: ActorRef[WithdrawReply]) extends Command
  sealed trait WithdrawReply
  final case object Withdrawn extends WithdrawReply
  final case object WithdrawFailed extends WithdrawReply

  def apply(balance: Long): Behavior[Command] = {
    Behaviors.receiveMessage {
      case Deposit(amount, to) =>
        to ! Deposited
        BankAccount(balance + amount)
      case GetBalance(to) =>
        to ! Balance(balance)
        Behavior.same
      case Withdraw(amount, to) =>
        if (balance < amount) {
          to ! WithdrawFailed
          Behaviors.same
        } else {
          to ! Withdrawn
          BankAccount(balance - amount)
        }
    }
  }
}
