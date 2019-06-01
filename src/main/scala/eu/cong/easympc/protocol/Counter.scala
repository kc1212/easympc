package eu.cong.easympc.protocol

import akka.actor.{Actor, Props}

object Counter {
  def prop(start: Int): Props = Props(new Counter(start))
}

class Counter(start: Int) extends Actor {
  private var count = start
  override def receive: Receive = {
    case "incr" => count += 1
    case "get"  => sender ! count
  }
}
