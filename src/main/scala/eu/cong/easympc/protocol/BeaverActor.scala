package eu.cong.easympc.protocol

import akka.actor.{Actor, ActorRef, Props}
import akka.pattern._
import akka.util.Timeout
import eu.cong.easympc.SecretSharing.Share
import eu.cong.easympc.AbGroupScalar.Ops
import eu.cong.easympc.protocol.DealerActor.Triple
import eu.cong.easympc.vm._

import scala.concurrent.Future
import scala.concurrent.duration._

object BeaverActor {

  def prop(roster: Set[ActorRef], dealer: ActorRef, prog: Iterable[Instruction[ActorRef]], shares: Map[ActorRef, Share]): Props =
    Props(new BeaverActor(roster, dealer, prog, shares))

  case object Start
  case class Result()

  private def transformProg[T](prog: List[Instruction[T]], shares: Map[T, Share]): List[Instruction[Share]] =
    prog map {
      case Push(x) => Push(shares(x))
      case Add()   => Add()
      case Mul()   => Mul()
    }
}

/**
  * The Beaver actor assumes perfect channels and each query to the dealer or
  * the storage actor will return a result.
  *
  * @param roster
  * @param dealer
  * @param prog
  * @param shares
  */
class BeaverActor(roster: Set[ActorRef], dealer: ActorRef, prog: Iterable[Instruction[ActorRef]], shares: Map[ActorRef, Share]) extends Actor {
  require(roster contains self)
  require(shares.values.map(_.x).toSet.size == 1)

  import BeaverActor._
  import context.dispatcher
  implicit val timeout = Timeout(5 seconds)

  override def receive: Receive = {
    case Start =>
    case _     =>
  }

  private def add(a: Share, b: Share, pc: Int): Future[Share] = {
    require(a.x == b.x, s"${a.x} not equal to ${b.x}")
    Future { Share(a.x, a.y <+> b.y) }
  }

  /**
    *
    * @param x_box
    * @param y_box
    * @param pc
    * @return
    */
  private def mul(x_box: Share, y_box: Share, pc: Int): Future[Share] = {
    require(x_box.x == y_box.x, s"${x_box.x} not equal to ${y_box.x}")
    for {
      triple <- dealer ? DealerActor.GetTriple(pc) map { _.asInstanceOf[Triple] }
      opening <- combineBeaverShares(triple.a, triple.b, roster - self)
    } yield {
      val z_box = triple.c.y <+> (x_box.y <*> opening._2) <+> (y_box.y <*> opening._1) <-> (opening._1 <*> opening._2)
      Share(x_box.x, z_box)
    }
  }

  private def combineBeaverShares(box_a: Share, box_b: Share, roster: Set[ActorRef]): Future[(BigInt, BigInt)] = {
    ???
  }
}
