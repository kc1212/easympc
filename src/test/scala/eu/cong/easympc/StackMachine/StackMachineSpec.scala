package eu.cong.easympc.StackMachine

import org.scalatest.{Matchers, WordSpecLike}

class StackMachineSpec extends WordSpecLike with Matchers {

  def addOp(l: Int, r: Int): Int = {
    l + r
  }

  def mulOp(l: Int, r: Int): Int = {
    l * r
  }

  def invalidOp(l: Int, r: Int): Int = {
    throw new IllegalArgumentException
  }

  "stack machine" must {
    "add integers" in {
      val instructions = List(Push(1), Push(2), Add())
      val result = StackMachine(instructions, addOp, invalidOp)
      result.get should equal(3)
    }
  }
}
