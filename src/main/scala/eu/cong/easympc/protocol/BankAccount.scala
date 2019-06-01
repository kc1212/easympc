package eu.cong.easympc.protocol

import akka.actor.{Actor, Props}

object BankAccount {
  def prop(initialBalance: Int): Props = Props(new BankAccount(initialBalance))

  case class Deposit(amount: Int)
  case class Withdraw(amount: Int)
  case object Balance
  case object Done
  case object Failed
}

class BankAccount(initialBalance: Int) extends Actor {
  require(initialBalance >= 0)
  import BankAccount._

  private var balance = initialBalance

  override def receive: Receive = {
    case Deposit(amount) =>
      balance += amount
      sender ! Done
    case Withdraw(amount) =>
      if (balance < amount) {
        sender ! Failed
      } else {
        balance -= amount
        sender ! Done
      }
    case Balance =>
      sender ! balance
    case _ =>
      sender ! Failed
  }
}
