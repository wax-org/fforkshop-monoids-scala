package wax.exercise.fibonacci

import cats.Monoid
import wax.utils.Benchmark

import scala.math.BigInt._
import scala.math.BigInt

object Main extends App {
  println("Matrix")
  bench(Fib.fibOtEn)

  println()

  println("Tail recursive")
  bench(Fib.fibTailRec)

  def bench(f: BigInt => BigInt): Unit =
    Seq(10, 100, 1000, 10000, 100000, 1000000, 10000000).foreach { en =>
      val res = Benchmark(f(en))
      println(s"$en -> ${res.duration.toMillis} ms")
    }
}

object Fib {

  implicit val matrixMonoid: Monoid[Matrix2x2] = new Monoid[Matrix2x2] {
    override def empty: Matrix2x2 = ???

    override def combine(x: Matrix2x2, y: Matrix2x2): Matrix2x2 = ???
  }

  def exp[T: Monoid](n: T, power: BigInt): T = ???

  case class Matrix2x2(a11: BigInt, a12: BigInt, a21: BigInt, a22: BigInt)

  object Matrix2x2 {
    def mult(x: Matrix2x2, y: Matrix2x2) = new Matrix2x2(
      x.a11 * y.a11 + x.a12 * y.a21, x.a11 * y.a12 + x.a12 * y.a22,
      x.a21 * y.a11 + x.a22 * y.a21, x.a21 * y.a12 + x.a22 * y.a22
    )
  }

  def fibOtEn(en: BigInt): BigInt = exp(
    Matrix2x2(
      1, 1,
      1, 0
    ),
    en
  ).a21

  private val Zero = BigInt(0)
  def fibTailRec(en: BigInt): BigInt = {
    @scala.annotation.tailrec
    def secretWeapon(n: BigInt, a: BigInt, b: BigInt): BigInt = n match {
      case Zero => a
      case _    => secretWeapon(n - 1, b, a + b)
    }

    secretWeapon(en, 0, 1)
  }
}
