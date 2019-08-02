package eu.cong.easympc.protocol

import akka.actor.{Actor, Props}

import scala.collection.mutable

object StorageActor {
  val prop: Props = Props[StorageActor]
  case class Set(k: Int, v: BigInt)
  case class SetOk(k: Int)
  case class Get(k: Int)
  case class Val(v: Option[BigInt])
}

class StorageActor extends Actor {
  import StorageActor._
  private val storage = mutable.Map[Int, BigInt]()
  override def receive: Receive = {
    case Set(k, v) =>
      storage put (k, v)
      sender ! SetOk(k)
    case Get(k) =>
      sender ! Val(storage get k)
  }
}
