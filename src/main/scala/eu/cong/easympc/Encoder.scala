package eu.cong.easympc

import org.bouncycastle.math.ec.ECPoint

import scala.util.{Success, Try}

trait Encoder[T] {
  // TODO consider using a stream
  def encode(x: T): Array[Byte]
  def decode(buf: Array[Byte]): Try[T]
}

object Encoder {
  def encode[T](x: T)(implicit enc: Encoder[T]): Array[Byte] = enc.encode(x)
  def decode[T](buf: Array[Byte])(implicit enc: Encoder[T]): Try[T] =
    enc.decode(buf)

  implicit class EncodableOps[T](val x: T)(implicit enc: Encoder[T]) {
    def encode(): Array[Byte] = enc.encode(x)
    def decode(buf: Array[Byte]): Try[T] = enc.decode(buf)
  }

  implicit val bigInt: Encoder[BigInt] = {
    new Encoder[BigInt] {
      override def encode(x: BigInt): Array[Byte] = x.toByteArray
      override def decode(buf: Array[Byte]): Try[BigInt] = Success(BigInt(buf))
    }
  }

  implicit val curve25519Point: Encoder[ECPoint] = {
    new Encoder[ECPoint] {
      private val spec = Spec.curve25519
      private val curve = spec.getCurve
      override def encode(x: ECPoint): Array[Byte] = x.getEncoded(true)
      override def decode(buf: Array[Byte]): Try[ECPoint] =
        Try(curve.decodePoint(buf))
    }
  }
}
