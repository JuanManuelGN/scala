package recursion_schemes.fixpoints

import recursion_schemes.fixpoints.listff.ListF.{ana, cata, in, out}
import recursion_schemes.fixpoints.listff.{ConsF, Fix, ListF, NilF}
import recursion_schemes.nofixpoints.list._
import recursion_schemes.nofixpoints.listf.ListF.{Coalgebra, FAlgebra}

/** Implementación de listas usando esquemas de recursión con puntos fijos
  */
object listff {

  sealed trait ListF[A]
  case class NilF[A]()                    extends ListF[A]
  case class ConsF[A](head: BigInt, a: A) extends ListF[A]
  final case class Fix[F[_]](unfix: F[Fix[F]])

  object ListF {

    def in: ListF[Fix[ListF]] => Fix[ListF]  = Fix(_)
    def out: Fix[ListF] => ListF[Fix[ListF]] = _.unfix

    trait Functor[F[_]] {
      def map[A, B](f: A => B): F[A] => F[B]
    }

    implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](f: A => B): ListF[A] => ListF[B] = {
        case NilF()      => NilF()
        case ConsF(h, t) => ConsF(h, f(t))
      }
    }

    def ana[F[_], R, A](coalgebra: Coalgebra[F, A], in: F[R] => R)(a: A)(implicit
        F: Functor[F]
    ): R =
      in(F.map(ana(coalgebra, in))(coalgebra(a)))

    def cata[F[_], R, A](algebra: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A =
      algebra(F.map(cata(algebra, out))(out(r)))
  }
}

object Runner_ extends App {
  def rangeCoalgebra: Coalgebra[ListF, BigInt] =
    n => if (n > 0) ConsF(n, n - 1) else NilF()
  def rangeF: BigInt => Fix[ListF] = ana(rangeCoalgebra, in)
  println(
    s"unfold using ana for range 1 to 5 ${rangeF(5)}"
  )

  def multiplyAlgebra: FAlgebra[ListF, BigInt] = {
    case NilF()      => 1
    case ConsF(h, t) => h * t
  }
  def multiplyF: Fix[ListF] => BigInt = cata(multiplyAlgebra, out)
  val ls: Fix[ListF] = Fix(ConsF(5,Fix(ConsF(4,Fix(ConsF(3,Fix(ConsF(2,Fix(ConsF(1,Fix(NilF())))))))))))

  println(
    s"multiplicar lista usando cata: ${multiplyF(ls)}"
  )
}
