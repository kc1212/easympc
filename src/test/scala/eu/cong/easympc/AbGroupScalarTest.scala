package eu.cong.easympc

import org.scalatest.WordSpec
import org.scalatestplus.scalacheck.Checkers
import spire.laws._

class AbGroupScalarTest extends WordSpec with Checkers {

  "a small scalar group" should {
    implicit val grp = AbGroupScalar.multiplicativeGroupFromOrder(13)
    implicit val arb = ArbitraryHelper.arbScalar(grp)

    val ruleSet = GroupLaws[BigInt].abGroup
    for ((id, prop) <- ruleSet.all.properties)
      "ensure " + id in {
        check(prop)
      }
  }

  "a curve25519 scalar group" should {
    implicit val grp = AbGroupScalar.curve25519Scalar
    implicit val arb = ArbitraryHelper.arbScalar(grp)

    val ruleSet = GroupLaws[BigInt].abGroup
    for ((id, prop) <- ruleSet.all.properties)
      "ensure " + id in {
        check(prop)
      }
  }
}
