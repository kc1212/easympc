package eu.cong.easympc

import scala.util.Random
import eu.cong.easympc.AbGroupScalar.Ops
import eu.cong.easympc.SecretSharing.Share

object BeaverTriples {
  def apply(t: Int, n: Int)(implicit g: AbGroupScalar, r: Random): LazyList[(Seq[Share], Seq[Share], Seq[Share])] = {
    val a = g.randElem()
    val b = g.randElem()
    apply(a, b, t, n)
  }

  def apply(a: BigInt, b: BigInt, t: Int, n: Int)(implicit g: AbGroupScalar, r: Random): LazyList[(Seq[Share], Seq[Share], Seq[Share])] = {
    require(n >= t)
    val c = a <*> b

    val abox = SecretSharing.share(a, t, n)
    val bbox = SecretSharing.share(b, t, n)
    val cbox = SecretSharing.share(c, t, n)

    (abox, bbox, cbox) #:: apply(t, n)
  }
}
