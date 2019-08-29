package eu.cong.easympc.protocol

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.{Behaviors, StashBuffer}

import scala.collection.mutable

object BulletinBoard {
  sealed trait Msg
  final case class Set(replyTo: ActorRef[SetResult], k: Int, v: BigInt) extends Msg
  final case class Get(replyTo: ActorRef[Val], k: Int) extends Msg
  final case class Val(v: BigInt) extends Msg

  sealed trait SetResult extends Msg
  final case class Ok(k: Int) extends SetResult
  final case class Fail(k: Int) extends SetResult

  def behavior(): Behavior[Msg] = Behaviors.setup { ctx =>
    val storage = mutable.Map[Int, BigInt]()
    val buffer = StashBuffer[Msg](capacity = 100)
    val inner: Behavior[Msg] =
      Behaviors.receive {
        case (ctx, Set(sender, k, v)) =>
          if (storage.contains(k)) {
            sender ! Fail(k)
            Behaviors.same
          } else {
            storage put (k, v)
            sender ! Ok(k)
            buffer.unstashAll(ctx, Behaviors.same)
          }
        case (_, msg @ Get(sender, k)) =>
          // If the value exists, the actor replies, otherwise stash it and try again later.
          storage get k match {
            case Some(v) => sender ! Val(v)
            case None    => buffer.stash(msg)
          }
          Behaviors.same
        case _ =>
          Behaviors.unhandled
      }
    inner
  }
}
