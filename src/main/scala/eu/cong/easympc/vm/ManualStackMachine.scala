package eu.cong.easympc.vm

import scala.collection.mutable

object ManualStackMachine {
  sealed trait Action[T]
  case class ManualAdd[T](l: T, r: T) extends Action[T]
  case class ManualMul[T](l: T, r: T) extends Action[T]
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
