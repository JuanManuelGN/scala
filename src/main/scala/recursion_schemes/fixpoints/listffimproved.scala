package recursion_schemes.fixpoints

import recursion_schemes.fixpoints.listffimproved.{Coalgebra, ConsF, Fix, ListF, NilF, ana}

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

  def ana[F[_], A](coalgebra: Coalgebra[F, A])(a: A)(implicit
      F: Functor[F]
  ): Fix[F] =
    Fix(F.map(ana(coalgebra))(coalgebra(a)))
}

object Runner__ extends App {
  def rangeCoalgebra: Coalgebra[ListF, BigInt] =
    n => if (n > 0) ConsF(n, n - 1) else NilF()
  def rangeF: BigInt => Fix[ListF] = ana(rangeCoalgebra)
  println(
    s"unfold using ana for range 1 to 5 ${rangeF(5)}"
  )
}
