package eu.cong.easympc.bgw

sealed trait Expr
case class Add(l: Expr, r: Expr) extends Expr
case class Mul(l: Expr, r: Expr) extends Expr
case class Var(x: Int) extends Expr
case class Val(x: Int) extends Expr
