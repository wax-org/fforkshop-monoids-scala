package wax.exercise.fibonacci

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Properties
import org.scalacheck.Prop._

import scala.collection.mutable.ListBuffer

object FibSpec extends Properties("Fibonacci") {

  fibIsFibProp("tailRecFib", Fib.fibTailRec)
  fibIsFibProp("matrixFib", Fib.fibOtEn)

  val enGen: Gen[BigInt] = Gen.choose(2, 1000).map(a => BigInt(a))

  def fibIsFibProp(name: String, f: BigInt => BigInt): ListBuffer[(String, Prop)] = {
    property("fib(0) == 0") = {
      f(0) =? 0
    }
    property("fib(1) == 1") = {
      f(1) =? 1
    }
    property(s"$name is fib") = {
      forAll(enGen) { en: BigInt =>
        val a = f(en - 2)
        val b = f(en - 1)
        val c = f(en)
        a + b == c
      }
    }
  }

}
