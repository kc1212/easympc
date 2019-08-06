package eu.cong.easympc.vm

import org.scalatest.{Matchers, WordSpec}

class ManualStackMachineSpec extends WordSpec with Matchers {
  import ManualStackMachine._
  def runner(prog: Iterable[Instruction[Int]]): Int = {
    val machine = new ManualStackMachine[Int](prog)
    var lastAction = machine.next(None)
    while (lastAction.isDefined) {
      val a = lastAction.get
      a match {
        case ManualAdd(l, r) =>
          lastAction = machine.next(Some(l + r))
        case ManualMul(l, r) =>
          lastAction = machine.next(Some(l * r))
      }
    }
    machine.result()
  }
  "manual stack machine" must {
    "add integers" in {
      val instructions = List(Push(1), Push(2), Add())
      runner(instructions) shouldBe 3
    }

    "multiple integers" in {
      val instructions = List(Push(3), Push(2), Mul())
      runner(instructions) shouldBe 6
    }
  }
}
