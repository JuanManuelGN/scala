package rock.cats

import cats.Applicative
import cats.Apply

/*
  FlatMap = Apply + flatMap method
  Cats Type Class hierarchy

    Semigroup      Functor  Semigroupal
        ^               ^   ^
      Monoid            Apply
                        ^   ^
                   FlatMap Applicative
                        ^   ^
                        Monad
 */
object Lesson15_FlatMap {

  trait MyFlatMap[M[_]] extends Apply[M] {
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]

    // TODO
    // hint: Apply extends Functor: use map
    def ap[A, B](wf: M[A => B])(wa: M[A]): M[B] = {
      flatMap(wa)(a => map(wf)(f => f(a)))
      //       |  |         /   \    \/
      //       |  |     M[A=>B] A=>B B
      //       |  |      \____  _____/
      //     M[A] A   ===>   M[B]
    }

  }

  trait MyMonad[M[_]] extends Applicative[M] with MyFlatMap[M] {
    override def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure(f(a)))
  }

  import cats.FlatMap
  import cats.syntax.flatMap._ // flatMap extension methods
  import cats.syntax.functor._ // map extension method

  def getPairs[M[_]: FlatMap, A, B](numbers: M[A], chars: M[B]): M[(A, B)] = for {
    n <- numbers
    c <- chars
  } yield (n, c)

  def main(args: Array[String]): Unit = {}

}
