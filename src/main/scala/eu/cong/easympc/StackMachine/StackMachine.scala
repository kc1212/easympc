package eu.cong.easympc.StackMachine

import scala.collection.mutable.Stack
import scala.util.{Failure, Success, Try}

object StackMachine {
  // TODO OP should handle errors
  type OP[T] = (T, T) => T
  def apply[T](prog: Iterable[Instruction[T]], addOp: OP[T], mulOp: OP[T]): Try[T] = {
    val stack = Stack[T]()
    for (inst <- prog) {
      inst match {
        case Push(x) =>
          stack.push(x)
        case Add() =>
          val l = stack.pop()
          val r = stack.pop()
          stack.push(addOp(l, r))
        case Mul() =>
          val l = stack.pop()
          val r = stack.pop()
          stack.push(mulOp(l, r))
      }
    }

    // at the end of the operation, we should only see one value on the stack
    if (stack.size != 1) {
      Failure(new ArithmeticException("final stack size is not 1"))
    } else {
      Success(stack.pop())
    }
  }
}
