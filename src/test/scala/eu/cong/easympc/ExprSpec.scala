package eu.cong.easympc

import org.scalatest.{Matchers, WordSpecLike}

class ExprSpec extends WordSpecLike with Matchers {
  "expression variable count" must {
    "succeed" in {
      Expr.GetVars(Mul[Int, String](Var(1), Add(Val("abc"), Var(2)))) should equal(Seq(Var(1), Var(2)))
    }
  }
}
