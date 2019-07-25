package eu.cong.easympc.StackMachine

import scala.concurrent.{ExecutionContext, Future}

object StackMachine {

  type OP[T] = (T, T) => Future[T]

  def apply[T](prog: Iterable[Instruction[T]], addOp: OP[T], mulOp: OP[T])(implicit ec: ExecutionContext): Future[T] = {
    // stack is used for chaining multiple futures
    var stack = Future { List[T]() }
    for (inst <- prog) {
      stack = inst match {
        case Push(x) =>
          stack map { x :: _ }
        case Add() =>
          for {
            s <- stack
            res <- addOp(s(0), s(1)) // take the first two elements
          } yield res :: s.drop(2)
        case Mul() =>
          for {
            s <- stack
            res <- mulOp(s(0), s(1))
          } yield res :: s.drop(2)
      }
    }

    // at the end of the operation, we should only see one value on the stack
    for {
      s <- stack
    } yield {
      if (s.size != 1) {
        throw new ArithmeticException("final stack size is not 1")
      } else {
        s.head
      }
    }
  }
}
