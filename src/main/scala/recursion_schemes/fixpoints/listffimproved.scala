package recursion_schemes.fixpoints

/** Implementación de listas usando esquemas de recursión con puntos fijos eliminando las funciones
  * in u out
  */
object listffimproved {
  sealed trait ListF[A]
  case class NilF[A]()                    extends ListF[A]
  case class ConsF[A](head: BigInt, a: A) extends ListF[A]
  final case class Fix[F[_]](unfix: F[Fix[F]])

  trait Functor[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }

  implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
    override def map[A, B](f: A => B): ListF[A] => ListF[B] = {
      case NilF()      => NilF()
      case ConsF(h, t) => ConsF(h, f(t))
    }
  }

  type Coalgebra[F[_], A] = A => F[A]
  type Algebra[F[_], A]   = F[A] => A

  def ana[F[_], A](coalgebra: Coalgebra[F, A])(a: A)(implicit F: Functor[F]): Fix[F] =
    Fix(F.map(ana(coalgebra))(coalgebra(a)))

  def cata[F[_], A](algebra: Algebra[F, A])(r: Fix[F])(implicit F: Functor[F]): A =
    algebra(F.map(cata(algebra))(r.unfix))
  
  def hylo[F[_]: Functor, A](algebra: Algebra[F, A])(coalgebra: Coalgebra[F, A])(a: A): A =
    cata(algebra)(ana(coalgebra)(a))
}

import recursion_schemes.fixpoints.listffimproved._

object Runner__ extends App {
  def rangeCoalgebra: Coalgebra[ListF, BigInt] = n => if (n > 0) ConsF(n, n - 1) else NilF()
  def rangeF: BigInt => Fix[ListF] = ana(rangeCoalgebra)

  def mulAlgebra: Algebra[ListF, BigInt] = {
    case NilF() => 1
    case ConsF(head, tail) => head * tail
  }
  def mulCata: Fix[ListF] => BigInt = cata(mulAlgebra)

  def mulHylo: BigInt => BigInt = hylo(mulAlgebra)(rangeCoalgebra)

  println(
    s"unfold using ana for range 1 to 5 = ${rangeF(5)}"
  )
  val fixLs: Fix[ListF] = Fix(ConsF(3, Fix(ConsF(2, Fix(ConsF(1, Fix(NilF())))))))
  println(
    s"fold using cata to multiply the range 1 to 3 = ${mulCata(fixLs)}"
  )
  println(
    s"unfold and fold using hylo to multiply the range 1 to 5 = ${mulHylo(5)}"
  )
}
