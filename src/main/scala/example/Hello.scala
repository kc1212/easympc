package example

import java.security.SecureRandom

import scala.util.Random

object Hello extends App {
  implicit val r: Random = new Random(new SecureRandom())
  import Group.curve25519
  import Group.{BinaryOps, base, rand}

  // key gen
  val x = rand()
  val h = base ** x

  // encryption
  val msg = base ** rand()
  val y = rand()
  val s = h ** y
  val c1 = base ** y
  val c2 = msg ++ s

  // decryption
  val s_receiver = c1 ** x
  val msg_out = c2 ++ s_receiver.negate()
  println("msg ok: " + (msg_out == msg))
}
