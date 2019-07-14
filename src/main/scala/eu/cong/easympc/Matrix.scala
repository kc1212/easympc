package eu.cong.easympc

import scala.reflect.ClassTag

class Matrix[T] private (underlying: Seq[Seq[T]]) {

  import eu.cong.easympc.Matrix._

  def mul(B: Matrix[T])(mulOp: (T, T) => T, addOp: (T, T) => T): Matrix[T] = {
    require(this.dim.cols == B.dim.rows)

    val ax = arrays
    val bx = B.arrays

    val result = for (row <- ax)
      yield
        for (col <- bx.transpose)
          yield row zip col map Function.tupled(mulOp(_, _)) reduceLeft (addOp(_, _))
    Matrix.array(result)
  }

  def add(other: Matrix[T])(implicit addOp: (T, T) => T): Matrix[T] = ???

  def dim: Dim = {
    val rows = underlying.size
    val cols = if (underlying.isEmpty) 0 else underlying(0).size
    Dim(rows, cols)
  }

  def arrays: Seq[Seq[T]] = underlying

  def t: Matrix[T] = Matrix.array(underlying.transpose)

  def det: T = ???

  def inverse: Matrix[T] = ???
}

object Matrix {

  case class Dim(rows: Int, cols: Int)

  case class ZeroElem[T](elem: T)

  def diag[T: ClassTag](xs: Seq[T])(implicit zero: ZeroElem[T]): Matrix[T] = {
    val z = zeros[T](xs.size, xs.size)
    val result = for (row <- z.arrays.zipWithIndex)
      yield
        row._1.zipWithIndex map { x =>
          if (x._2 == row._2) xs(row._2) else zero.elem
        }
    new Matrix(result)
  }

  def array[T](xs: Seq[Seq[T]]): Matrix[T] = {
    require(Set(xs map { _.size }).size == 1, "all rows must have the same size")
    new Matrix(xs)
  }

  def zeros[T: ClassTag](rows: Int, cols: Int)(implicit zero: ZeroElem[T]): Matrix[T] =
    new Matrix((0 until rows).map { _ =>
      Vector.fill[T](cols)(zero.elem)
    })

}
