package eu.cong.easympc

import org.bouncycastle.jce.ECNamedCurveTable
import org.bouncycastle.jce.spec.ECNamedCurveParameterSpec

object Spec {
  val curve25519: ECNamedCurveParameterSpec =
    ECNamedCurveTable.getParameterSpec("curve25519")
}
