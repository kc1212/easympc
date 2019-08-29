package eu.cong.easympc.vm

import scala.collection.mutable

object ManualStackMachine {
  sealed trait Action[T]
  case class ManualAdd[T](l: T, r: T) extends Action[T]
  case class ManualMul[T](l: T, r: T) extends Action[T]

  case class State[T](stack: List[T], pc: Int)

  def apply[T](prevResult: Option[T], state: State[T])(implicit prog: List[Instruction[T]]): (Option[Action[T]], State[T]) = {
    val stack = prevResult match {
      case Some(x) =>
        x :: state.stack
      case None =>
        state.stack
    }

    if (state.pc >= prog.size) {
      return (None, State(stack, state.pc))
    }

    prog(state.pc) match {
      case Push(x) =>
        apply(None, State(x :: stack, state.pc + 1))
      case Add() =>
        (Some(ManualAdd(stack(0), stack(1))), State(stack.drop(2), state.pc + 1))
      case Mul() =>
        (Some(ManualMul(stack(0), stack(1))), State(stack.drop(2), state.pc + 1))
    }
  }

  def result[T](state: State[T]): T =
    if (state.stack.size != 1) {
      throw new ArithmeticException("final stack size is not 1")
    } else {
      state.stack.head
    }
}

class ManualStackMachine[T](prog: Iterable[Instruction[T]]) {
  import ManualStackMachine._
  private val stack = mutable.Stack[T]()
  private val progList = prog.toList
  private var pc = 0

  def next(prevResult: Option[T]): Option[Action[T]] = {
    prevResult match {
      case Some(x) => stack.push(x)
      case None    => // do nothing
    }

    if (pc >= progList.size) {
      return None
    }

    val inst = progList(pc)
    pc += 1

    inst match {
      case Push(x) =>
        stack.push(x)
        next(None) // should be tail recursive?
      case Add() =>
        val l = stack.pop()
        val r = stack.pop()
        Some(ManualAdd(l, r))
      case Mul() =>
        val l = stack.pop()
        val r = stack.pop()
        Some(ManualMul(l, r))
    }
  }

  def result(): T = {
    if (stack.size != 1) {
      throw new ArithmeticException("final stack size is not 1")
    } else {
      stack.head
    }
  }
}
