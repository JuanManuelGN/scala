package rock.cats

/*
  Applicatives
  = Functors + pure method
  
  Not quite Monads
  - example: Validated
  
  Cats Type Class hierarchy
  
    Semigroup      Functor  Semigroupal
        ^               ^   ^
      Monoid         Applicative
                          ^
                        Monad
*/
class Lesson13Applicatives {

  // Applicatives = Functors + the pure method
  import cats.Applicative
  import cats.instances.list._
  val listApplicative = Applicative[List]
  val aList           = listApplicative.pure(2) // List(2)

  import cats.instances.option._ // import Applicative[Option]
  val optionApplicative = Applicative[Option]
  val anOption          = optionApplicative.pure(2) // Some(2)

  // pure extension method
  import cats.syntax.applicative._
  val aSweetList   = 2.pure[List]   // List(2)
  val aSweetOption = 2.pure[Option] // Some(2)

  // Monads extend Applicative
  // Applicative extend Functors
  import cats.data.Validated
  type ErrorOr[T] = Validated[List[String], T]
  val aValidValue: ErrorOr[Int]        = Validated.valid(43)    // pure
  val aModifiedValidated: ErrorOr[Int] = aValidValue.map(_ + 1) // map from Functor
  val validatedApplicative             = Applicative[ErrorOr]

  // TODO: thought experiment. Implement the following method given an implicit instance of Applicative[W]
  def productWithApplicative[W[_], A, B](wa: W[A], wb: W[B])(implicit
      applicative: Applicative[W]
  ): W[(A, B)] = {
    // no tengo flatMap
    // applicative.map(wa)(a => applicative.map(wb)(b => (a, b)))
    // Es imposible hacerlo, se necesita el método ap

    val wf = applicative.map(wa)(a => (b: B) => (a, b) /* function */)
    val result = applicative.ap(wf)(wb)
    result
    // Con esta función un Applicative puede implementar product de Semigroupal,
    // por lo tanto Applicatives puede extender Semigroupal
  }
  
  def ap[W[_], A, B](wf: W[A => B])(wa: W[A]): W[B] = ??? // this already implemented

  def main(args: Array[String]): Unit = {}

}
