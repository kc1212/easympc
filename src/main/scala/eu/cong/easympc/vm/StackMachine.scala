package eu.cong.easympc.vm

import scala.concurrent.{ExecutionContext, Future}

object StackMachine {

  type OP[T] = (T, T, Int) => Future[T]

  def apply[T](prog: Iterable[Instruction[T]], addOp: OP[T], mulOp: OP[T])(implicit ec: ExecutionContext): Future[T] = {
    val finalStack = prog.zipWithIndex.foldLeft(Future { List[T]() }) { (stack, instWithPC) =>
      val inst = instWithPC._1
      val pc = instWithPC._2 // program counter
      inst match {
        case Push(x) =>
          stack map { x :: _ }
        case Add() =>
          for {
            s <- stack
            res <- addOp(s(0), s(1), pc)
          } yield res :: s.drop(2)
        case Mul() =>
          for {
            s <- stack
            res <- mulOp(s(0), s(1), pc)
          } yield res :: s.drop(2)
      }
    }

    // at the end of the operation, we should only see one value on the stack
    for {
      stack <- finalStack
    } yield {
      if (stack.size != 1) {
        throw new ArithmeticException("final stack size is not 1")
      } else {
        stack.head
      }
    }
  }
}
