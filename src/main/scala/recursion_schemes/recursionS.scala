package recursion_schemes

import cats.Functor
import cats.implicits._
import recursion_schemes.recursionS.Stack.{done, more}

/**
  * https://free.cofree.io/2017/11/13/recursion/
  */
object recursionS {

  final case class Fix[F[_]](unfix: F[Fix[F]])

  sealed trait Stack[A]

  final case class Done[A](result: Int) extends Stack[A]

  final case class More[A](a: A, next: Int) extends Stack[A]

  object Stack {
    implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
      override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
        sa match {
          case Done(result) => Done(result)
          case More(a, next) => More(f(a), next)
        }
    }

    def done[A](result: Int = 1): Stack[A] = Done(result)

    def more[A](a: A, next: Int): Stack[A] = More(a, next)
  }

  // Type A => F[A] is also known as Coalgebra.
  def ana[F[_] : Functor, A](coAlgebra: A => F[A]): A => Fix[F] =
    a => Fix(coAlgebra(a) map ana(coAlgebra))

  // Type F[A] => A is also known as Algebra.
  def cata[F[_] : Functor, A](algebra: F[A] => A): Fix[F] => A =
    fix => algebra(fix.unfix map cata(algebra))

  // Coalgebra for factorial
  val stackCoalgebra: Int => Stack[Int] =
    n => if (n > 0) more(n - 1, n) else done()

  // Algebra for factorial
  val stackAlgebra: Stack[Int] => Int = {
    case Done(result) => result
    case More(acc, next) => acc * next
  }
}

import recursion_schemes.recursionS.{ana, cata, stackAlgebra, stackCoalgebra}
object Runner extends App {
  /**
    * def ana[F[_] : Functor, A](coAlgebra: A => F[A]): A => Fix[F] =
    *   a => Fix(coAlgebra(a) map ana(coAlgebra))
    * implicit val stackFunctor: Functor[Stack] = new Functor[Stack] {
    *   override def map[A, B](sa: Stack[A])(f: A => B): Stack[B] =
    *     sa match {
    *       case Done(result) => Done(result)
    *       case More(a, next) => More(f(a), next)
    *     }
    * ana(stackCoalgebra).apply(5) =
    * Fix(More(Fix(More(Fix(More(Fix(More(Fix(More(Fix(Done(1)),1)),2)),3)),4)),5))
    * Step by Step
    * ana(stackCoalgebra).apply(5) = { ana = a => Fix(f(a) map ana(f)) | a = 5 | f = stackCoalgebra }
    *  Fix(stackCoalgebra(5) map ana(stackCoalgebra)) = { stackCoalgebra(5) = more(4, 5) }
    *   Fix(more(4, 5) map ana(stackCoalgebra)) = { more(4, 5) = More(4, 5) }
    *    Fix(More(4, 5) map ana(stackCoalgebra)) = { map ana(stackCoalgebra) = More(ana(stackCoalgebra)(a), next) | a = 4 | next = 5}
    *     Fix(More(ana(stackCoalgebra)(4), 5)) // aquí se produce la llamada recursiva a ana(...
    *      ...
    *
    */
  println(
    ana(stackCoalgebra).apply(5)
  )

  println(
    (ana(stackCoalgebra) andThen cata(stackAlgebra))(5)
  )
}