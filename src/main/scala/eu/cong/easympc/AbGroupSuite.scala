package eu.cong.easympc

trait AbGroupSuite[P] {
  val pointGroup: AbGroupPoint[P]
  val scalarGroup: AbGroupScalar
}

object AbGroupSuite {
  def apply[P](p: AbGroupPoint[P]): AbGroupSuite[P] =
    new AbGroupSuite[P] {
      override val pointGroup: AbGroupPoint[P] = p

      override val scalarGroup: AbGroupScalar = AbGroupScalar.multiplicativeGroupFromOrder(p.order)
    }
}
