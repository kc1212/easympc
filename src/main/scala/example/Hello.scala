package example
import Suite._

import scala.util.Random

object Suite {
  implicit val g: Group[BigInt, BigInt] = Group.bigIntGroup
  implicit val r: Random = new Random(123)
}

object Hello extends App {
  import Group.{BinaryOps, base, rand}

  // key gen
  val x = rand()
  val h = base ** x

  // encryption
  val msg = rand()
  val y = rand()
  val s = h ** y
  val c1 = base ** y
  val c2 = msg ++ s

  // decryption
  val s_receiver = c1 ** x
  val msg_out = c2 ++ s_receiver.negate()
  println("msg ok: " + (msg_out == msg))
}
