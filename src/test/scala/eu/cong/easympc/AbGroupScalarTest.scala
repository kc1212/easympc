package eu.cong.easympc

import org.scalatest.funsuite.AnyFunSuite
import org.typelevel.discipline.scalatest.Discipline
import spire.laws._

class AbGroupScalarTest extends AnyFunSuite with Discipline {
  {
    implicit val grp = AbGroupScalar.fromOrder(13)
    implicit val arb = ArbitraryHelper.arbScalar(grp)
    checkAll("small group", GroupLaws[BigInt].abGroup(grp))
  }
  {
    implicit val grp = AbGroupScalar.curve25519Scalar
    implicit val arb = ArbitraryHelper.arbScalar(grp)
    checkAll("big group", GroupLaws[BigInt].abGroup(grp))
  }
}
