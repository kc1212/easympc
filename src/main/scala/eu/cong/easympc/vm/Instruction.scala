package eu.cong.easympc.vm

sealed trait Instruction[+T]
case class Add[T]() extends Instruction[T]
case class Mul[T]() extends Instruction[T]
case class Push[T](x: T) extends Instruction[T] {
  def inner: T = x
}
// case class Const() extends Instruction
