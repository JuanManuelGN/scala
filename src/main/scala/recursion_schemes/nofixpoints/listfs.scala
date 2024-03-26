package recursion_schemes.nofixpoints

import recursion_schemes.nofixpoints.lists._

object listfs {

  sealed trait ListF[A]
  case class NilF[A]()                    extends ListF[A]
  case class ConsF[A](head: BigInt, a: A) extends ListF[A]

  object ListF {
    def in: ListF[List] => List = {
      case NilF()      => Nil
      case ConsF(h, t) => Cons(h, t)
    }

    def out: List => ListF[List] = {
      case Nil        => NilF()
      case Cons(h, t) => ConsF(h, t)
    }

    trait Functor[F[_]] {
      def map[A, B](f: A => B): F[A] => F[B]
    }

    implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
      override def map[A, B](f: A => B): ListF[A] => ListF[B] = {
        case NilF()      => NilF()
        case ConsF(h, t) => ConsF(h, f(t))
      }
    }

    type FAlgebra[F[_], A]  = F[A] => A
    type Coalgebra[F[_], A] = A => F[A]

    def inF: FAlgebra[ListF, List] = {
      case NilF()      => Nil
      case ConsF(h, t) => Cons(h, t)
    }

    def ana[F[_], R, A](coalgebra: Coalgebra[F, A], in: F[R] => R)(a: A)(implicit
        F: Functor[F]
    ): R =
      in(F.map(ana(coalgebra, in))(coalgebra(a)))

    def cata[F[_], R, A](algebra: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A =
      algebra(F.map(cata(algebra, out))(out(r)))

    def hylo[F[_], A, B](coalgebra: A => F[A], algebra: F[B] => B)(a: A)(implicit
        F: Functor[F]
    ): B =
      algebra(F.map(hylo(coalgebra, algebra))(coalgebra(a)))
  }

}
