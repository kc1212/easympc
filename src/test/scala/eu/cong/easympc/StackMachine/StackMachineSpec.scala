package eu.cong.easympc.StackMachine

import org.scalatest.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future

class StackMachineSpec extends AsyncWordSpec with Matchers {

  def addOp(l: Int, r: Int): Future[Int] = Future { l + r }

  def mulOp(l: Int, r: Int): Future[Int] = Future { l * r }

  def invalidOp(l: Int, r: Int): Future[Int] = throw new IllegalArgumentException

  "stack machine" must {
    "add integers" in {
      val instructions = List(Push(1), Push(2), Add())
      for {
        result <- StackMachine(instructions, addOp, invalidOp)
      } yield result shouldBe 3
    }

    "multiply integers" in {
      val instructions = List(Push(3), Push(2), Mul())
      for {
        result <- StackMachine(instructions, invalidOp, mulOp)
      } yield result shouldBe 6
    }

    "add and multiply integers" in {
      val instructions = List(Push(3), Push(2), Mul(), Push(1), Add())
      for {
        result <- StackMachine(instructions, addOp, mulOp)
      } yield result shouldBe 7
    }

    "fail if the instructions are wrong" in {
      // TODO correctly handle the error
      // val instructions = List(Push(2), Mul())
      // val result = StackMachine(instructions, invalidOp, mulOp)
      succeed
    }
  }
}
