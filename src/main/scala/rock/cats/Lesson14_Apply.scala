package rock.cats

import cats.{Functor, Semigroupal}

/*
  Apply = Functor + Semigroupal + ap method
  
  Cats Type Class hierarchy
  
    Semigroup      Functor  Semigroupal
        ^               ^   ^
      Monoid            Apply
                          ^
                     Applicative
                          ^
                        Monad
 */
object Lesson14_Apply {

  trait MyApplicative[W[_]] extends Functor[W] with Semigroupal[W] {
    def pure[A](x: A): W[A]

    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] = ???
  }

  trait MyApply[W[_]] extends Functor[W] with Semigroupal[W] {
    override def product[A, B](fa: W[A], fb: W[B]): W[(A, B)] = {
      val functionWrapper: W[B => (A, B)] = map(fa)(a => (b: B) => (a, b))
      ap(functionWrapper)(fb)
    }

    def ap[B, T](wf: W[B => T])(wb: W[B]): W[T] = ???

    // TODO implement mapN
    def mapN[A, B, C](tuple: (W[A], W[B]))(f: (A, B) => C): W[C] = {
      val wrapperT: W[(A, B)] = product(tuple._1, tuple._2)
      map(wrapperT) { case (a, b) => f(a, b) }
    }
  }

  trait MyApplicativeWithApply[W[_]] extends MyApply[W] {
    def pure[A](x: A): W[A]
  }

  import cats.Applicative
  import cats.Apply
  import cats.instances.option._ // implicit Apply[Option]
  val applyOption = Apply[Option]
  val funcApp     = applyOption.ap(Some((x: Int) => x + 1))(Some(2)) // Some(3)
  val funcApp2    = applyOption.ap(Some((x: Int) => x + 1))(_)

  // extension methods
  import cats.syntax.apply._
  val tupleOfOptions = (Option(1), Option(3), Option(3))
  val optionOfTuple  = tupleOfOptions.tupled          // Some((1,2,3))
  val sumOption      = tupleOfOptions.mapN(_ + _ + _) // Some(6)

  def main(arg: Array[String]): Unit = {
    println(funcApp)
    println(funcApp2(Some(4)))
    println(funcApp2(None))
  }
}
