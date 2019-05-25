package example
import Suite._

object Suite {
  case class R() extends Rand {
    var i = 0
    def nextInt(): Int = {
      i+=1
      i
    }
  }
  implicit val r = R()
  implicit val g = Group.bigIntGroup
}

object Hello extends App {
  import Group.{BinaryOps, base, rand}

  // key gen
  val x = BigInt(17)
  val h = base ** x

  // encryption
  val msg = BigInt(542)
  val y = BigInt(5555)
  val s = h ** y
  val c1 = base ** y
  val c2 = msg ++ s

  // decryption
  val s_receiver = c1 ** x
  val msg_out = c2 ++ s_receiver.negate()
  println("msg is: " +  msg_out)
}
