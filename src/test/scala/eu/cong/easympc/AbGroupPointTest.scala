package eu.cong.easympc

import org.bouncycastle.math.ec.ECPoint
import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.laws.GroupLaws

class AbGroupPointTest extends AnyFunSuite with Discipline {
  {
    implicit val grp = AbGroupPoint.bigIntGroup
    implicit val arb = ArbitraryHelper.arbPoint(grp)
    checkAll("integer group", GroupLaws[BigInt].abGroup(grp))
  }

  {
    implicit val grp = AbGroupPoint.curve25519Point
    implicit val arb = ArbitraryHelper.arbPoint(grp)
    checkAll("curve25519 point group", GroupLaws[ECPoint].abGroup(grp))
  }
}
