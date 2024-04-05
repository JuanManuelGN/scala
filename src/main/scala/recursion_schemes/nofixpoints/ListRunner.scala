package recursion_schemes.nofixpoints

import recursion_schemes.nofixpoints.list._
import recursion_schemes.nofixpoints.listf._
import recursion_schemes.nofixpoints.listf.ListF._

/** https://www.youtube.com/watch?v=XZ9nPZbaYfE
  */
object listFunctions {

  def multiplyAlgebra: FAlgebra[ListF, BigInt] = {
    case NilF()      => 1
    case ConsF(h, t) => h * t
  }
  def multiplyF: List => BigInt = cata(multiplyAlgebra, out)

  def range: BigInt => List = // ana
    n => if (n > 0) Cons(n, range(n - 1)) else Nil

  def rangeCoalgebra: Coalgebra[ListF, BigInt] =
    n => if (n > 0) ConsF(n, n - 1) else NilF()

  def rangeF: BigInt => List = ana(rangeCoalgebra, inF)

  def factorial: BigInt => BigInt = n => // hylomorphism
    if (n > 0) n * factorial(n - 1) else 1

  def factorialF: BigInt => BigInt = hylo(rangeCoalgebra, multiplyAlgebra)

}

object Runner extends App {
  val ls = Cons(3, Cons(2, Cons(1, Nil)))

  println(
    s"list $ls}"
  )

  println(
    s"list mul classical recursion style ${List.multiply(ls)}"
  )
  println(
    s"list mul tail recursion style ${List.multiplyTR(ls = ls)}" // si la lista es Nil falla
  )
  println(
    s"list mul fold style ${List.multiplyFold(ls)}"
  )
  println(
    s"multiplicar lista usando cata: ${listFunctions.multiplyF(ls)}"
  )

  println(
    s"list length classical recursion style ${List.length(ls)}"
  )
  println(
    s"list length fold style ${List.lengthFold(ls)}"
  )

  println(
    s"unfold using ana for range 1 to 5 ${listFunctions.rangeF(5)}"
  )

  println(
    s"unfold and fold to calculate factorial for 5 using hylo ${listFunctions.factorialF(5)}"
  )

}
