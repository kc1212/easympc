package eu.cong.easympc.protocol

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import akka.util.Timeout
import eu.cong.easympc.AbGroupScalar
import eu.cong.easympc.SecretSharing.Share
import eu.cong.easympc.AbGroupScalar.Ops
import eu.cong.easympc.vm._

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success}

object BeaverProtocol {
  sealed trait Msg
  final case object Start extends Msg
  final case class Result() extends Msg
  private final case class AdaptedTriple(a: Share, b: Share, c: Share) extends Msg

  private def transformProg[T](prog: List[Instruction[T]], shares: Map[T, Share]): List[Instruction[Share]] =
    prog map {
      case Push(x) => Push(shares(x))
      case Add()   => Add()
      case Mul()   => Mul()
    }
}

class BeaverProtocol(dealer: ActorRef[Dealer.GetTriple] /*, prog: Iterable[Instruction[ActorRef]], shares: Map[ActorRef, Share]*/ )(
    implicit g: AbGroupScalar,
    r: Random) {
  // require(shares.values.map(_.x).toSet.size == 1)

  import BeaverProtocol._
  implicit val timeout: Timeout = 5.seconds

  /*
  val behavior: Behavior[Msg] = Behaviors.setup { ctx =>
    implicit val ec = ctx.executionContext

    def add(a: Share, b: Share, pc: Int): Future[Share] = {
      require(a.x == b.x, s"${a.x} not equal to ${b.x}")
      Future { Share(a.x, a.y <+> b.y) }
    }

    def mul(x_box: Share, y_box: Share, pc: Int): Future[Share] = {
      require(x_box.x == y_box.x, s"${x_box.x} not equal to ${y_box.x}")
      ctx.ask
      ctx.ask(dealer)(Dealer.GetTriple(_, pc, pc)) {
        case Success(Dealer.Triple(a, b, c)) => AdaptedTriple(a, b, c)
        case Failure(e)                      => throw new RuntimeException(e)
      }

      ???

      /*
      for {
        triple <- dealer.ask(ref => Dealer.GetTriple(ref, pc, pc)) map { _.asInstanceOf[Triple] }
        opening <- combineBeaverShares(triple.a, triple.b, roster - self)
      } yield {
        val z_box = triple.c.y <+> (x_box.y <*> opening._2) <+> (y_box.y <*> opening._1) <-> (opening._1 <*> opening._2)
        Share(x_box.x, z_box)
      }
 */
    }

    val inner: Behavior[Msg] = Behaviors.receiveMessage {
      ???
    }

    inner
  }

  private def combineBeaverShares(box_a: Share, box_b: Share, roster: Set[ActorRef]): Future[(BigInt, BigInt)] = {
    ???
  }
 */
}
