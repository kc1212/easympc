package eu.cong.easympc

/** This trait represents an algebraic expression.
  * @tparam V is the type of the variable
  * @tparam C is the type of the constant
  */
sealed trait Expr[V, C]
case class Add[V, C](l: Expr[V, C], r: Expr[V, C]) extends Expr[V, C]
case class Mul[V, C](l: Expr[V, C], r: Expr[V, C]) extends Expr[V, C]
case class Var[V, C](x: V) extends Expr[V, C]
case class Val[V, C](x: C) extends Expr[V, C]

object Expr {
  def GetVars[V, C](expr: Expr[V, C]): Seq[Var[V, C]] = {
    expr match {
      case Add(l, r)  => GetVars(l) ++ GetVars(r)
      case Mul(l, r)  => GetVars(l) ++ GetVars(r)
      case v @ Var(_) => List(v)
      case Val(_)     => List()
    }
  }
}
