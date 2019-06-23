package eu.cong.easympc

import scala.util.Random

trait Rand[T] {
  def randElem()(implicit r: Random): T
}
