package eu.cong.easympc.protocol

import akka.actor.typed.{ActorRef, Behavior}
import akka.actor.typed.scaladsl.Behaviors
import eu.cong.easympc.AbGroupScalar
import eu.cong.easympc.AbGroupScalar.Ops
import eu.cong.easympc.SecretSharing.Share
import eu.cong.easympc.vm._

import scala.collection.mutable
import scala.util.Random

object BeaverProtocol {
  sealed trait Msg
  final case object Start extends Msg
  final case class Result(id: Int, share: Share) extends Msg
  private final case class AdaptedTriple(inner: Dealer.Triple) extends Msg
  private final case class AdaptedSetResult(inner: BulletinBoard.SetResult) extends Msg
  private final case class AdaptedVal(inner: BulletinBoard.Val) extends Msg

  private def transformProg[T](prog: Iterable[Instruction[T]], shares: Map[T, Share]): Iterable[Instruction[Share]] =
    prog map {
      case Push(x) => Push(shares(x))
      case Add()   => Add()
      case Mul()   => Mul()
    }

  private class MsgCache() {
    // TODO clean old data
    private val buf = mutable.Map()
    def addTriple(triple: Dealer.Triple): Unit = ???
    def addOpening(index: Int, opening: (BigInt, BigInt)): Unit = ???
    def getTriple(index: Int): Option[Dealer.Triple] = ???
    def getOpening(index: Int): Option[(BigInt, BigInt)] = ???
  }
}

class BeaverProtocol(id: PartyID,
                     dealer: ActorRef[Dealer.GetTriple],
                     storage: ActorRef[BulletinBoard.Get],
                     roster: Set[PartyID],
                     prog: Iterable[Instruction[PartyID]],
                     shares: Map[PartyID, Share])(implicit g: AbGroupScalar, r: Random) {
  require(roster.map(_.x).toSet.size == 1)
  require(shares.values.map(_.x).toSet.size == 1)
  require(roster.size == shares.size)

  import BeaverProtocol._

  val behavior: Behavior[Msg] = Behaviors.setup { ctx =>
    val machine = new ManualStackMachine(transformProg(prog, shares))
    val cache = new MsgCache()
    var mulCounter = 0

    def step(a: Option[ManualStackMachine.Action[Share]]): Unit = {
      var action = a
      var multiplied = false
      while (action.isDefined && multiplied) {
        action.get match {
          case ManualStackMachine.ManualAdd(l, r) =>
            require(l.x == r.x, s"${l.x} not equal to ${r.x}")
            action = machine.next(Some(Share(l.x, l.y <+> r.y)))
          case ManualStackMachine.ManualMul(l, r) =>
            require(l.x == r.x, s"${l.x} not equal to ${r.x}")
            // get triples and get opening
            dealer ! Dealer.GetTriple(ctx.messageAdapter(AdaptedTriple), mulCounter, id)
            storage ! BulletinBoard.Get(ctx.messageAdapter(AdaptedVal), mulCounter)
            mulCounter += 1
            multiplied = true
        }
      }
    }

    /*
    def tryMul(): Unit = {
      val pair = for {
        o <- cache.getOpening(mulCounter)
        t <- cache.getTriple(mulCounter)
      } yield (o, t)
      pair match {
        case Some((opening, triple)) =>
          val z_box = triple.c.y <+> (x_box.y <*> opening._2) <+> (y_box.y <*> opening._1) <-> (opening._1 <*> opening._2)
          machine.next(Some(Share(x_box.x, z_box)))
        case None =>
      }
    }
     */

    val inner: Behavior[Msg] = Behaviors.receiveMessage {
      case Start =>
        step(machine.next(None))
        Behaviors.same
      case AdaptedTriple(inner) =>
        // TODO put into cache and see if we can use the latset mulCounter
        cache.addTriple(inner)
        Behaviors.same
      case AdaptedSetResult(inner) =>
        // TODO
        Behaviors.same
      case AdaptedVal(inner) =>
        // TODO
        Behaviors.same
      case _ =>
        Behaviors.unhandled
    }

    inner
  }
}
