package eu.cong.easympc

import eu.cong.easympc.Group.{TFe, TPt}

import scala.util.{Success, Try}

trait Encoder[T] {
  def encode(x: T): Array[Byte]
  def decode(buf: Array[Byte]): Try[T]
}

object Encoder {
  def encode[T](x: T)(implicit enc: Encoder[T]): Array[Byte] = enc.encode(x)
  def decode[T](buf: Array[Byte])(implicit enc: Encoder[T]): Try[T] =
    enc.decode(buf)

  implicit class EncodableOps[T](x: T)(implicit enc: Encoder[T]) {
    def encode(): Array[Byte] = enc.encode(x)
    def decode(buf: Array[Byte]): Try[T] = enc.decode(buf)
  }

  implicit val bigInt: Encoder[BigInt] = {
    new Encoder[BigInt] {
      override def encode(x: BigInt): Array[Byte] = x.toByteArray
      override def decode(buf: Array[Byte]): Try[BigInt] = Success(BigInt(buf))
    }
  }

  implicit val curve25519Point: Encoder[Group.TPt] = {
    new Encoder[TPt] {
      override def encode(x: TPt): Array[Byte] = ???
      override def decode(buf: Array[Byte]): Try[TPt] = ???
    }
  }

  implicit val curve25519Scalar: Encoder[Group.TFe] = {
    new Encoder[TFe] {
      override def encode(x: TFe): Array[Byte] = ???
      override def decode(buf: Array[Byte]): Try[TFe] = ???
    }
  }
}
