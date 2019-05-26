package eu.cong.easympc

import java.security.SecureRandom

import org.scalatest._

import scala.util.Random

class GroupSpec extends FlatSpec with Matchers {
  implicit val r: Random = new Random(new SecureRandom())
  import Group.{BinaryOps, base, rand}

  def elgamal[P, S](msg: P)(implicit g: Group[P, S]): Boolean = {
    // key gen
    val x = rand()
    val h = base ** x

    // encryption
    val y = rand()
    val s = h ** y
    val c1 = base ** y
    val c2 = msg ++ s

    // decryption
    val s_receiver = c1 ** x
    val msg_receiver = c2 ++ s_receiver.negate()
    msg_receiver == msg
  }

  "curve25519 group" should "do ElGamal" in {
    import Group.curve25519
    val msg = base ** rand() // quickcheck
    elgamal(msg) should be(true)
  }

  "big int group" should "do ElGamal" in {
    import Group.bigIntGroup
    val msg = base ** rand() // quickcheck
    elgamal(msg) should be(true)
  }
}
