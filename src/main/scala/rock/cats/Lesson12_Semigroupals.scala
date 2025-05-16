package rock.cats

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/*
  Higher-Kinded type class which can tuple elements
  Monads extends Semigroupals
    - product is implemented in terms of map/flatMap
  Some Semigroupals ares useful without being monads
    - example: Validated
  Don't confuse Semigroup with Semigroupal
     combining Vs tupling
  Cats Type Class hierarchy
  
    Semigroup      Functor  Semigroupal
        ^               ^   ^
     Monoid             Monad
 */
object Lesson12_Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption =
    optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTuple = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(
    Future("the meaning of life"),
    Future(42)
  ) // Future(("the meaning of life", 42))

  // Para qué sirve
  import cats.instances.list._
  val aTupledList = Semigroupal[List].product(
    List(1, 2),
    List("a", "b")
  ) // List((1, "a"), (1, "b"), (2, "a"), (2, "b"))

  // TODO: implement product with monads
  import cats.Monad
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    monad.flatMap(fa)(a => monad.map(fb)(b => (a, b)))

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  def productWithMonadsFor[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  trait MyMonad[M[_]] extends MySemigroupal[M] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    def map[A, B](ma: M[A])(f: A => B): M[B]         = flatMap(ma)(a => pure[B](f(a)))
    def product[A, B](fa: M[A], fb: M[B]): M[(A, B)] = flatMap(fa)(a => map(fb)(b => (a, b)))
  }

  // Monads extends Semigroupal. Monads secuencia las computaciones

  // use case for Semigroupal is Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Somethind wrong", "Something else wrong")),
    Validated.invalid(List("This can't be right"))
  ) // Invalid(List(Somethind wrong, Something else wrong, This can't be right))

  // Si usamos una Monad para esto no obtenemos el mismo resultado
  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // implemented in terms of map/flatMap
    Left(List("Somethind wrong", "Something else wrong")),
    Left(List("This can't be right"))
  ) // Left(List(Somethind wrong, Something else wrong)) no da el mismo resultado porque el Either al evaluar el primer argumento hace corto-circuito y termina ahí porque es un Left

  // m: Monad => Associativity law: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g)) This is true for Either but not true for Validated

  // TODO: define a Semigroupal[List] which does a zip
  // my solution. No es correcto porque hace el producto cartesiano, y lo que queremos es la función zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](
        fa: List[A],
        fb: List[B]
    ): List[(A, B)] = fa.flatMap(a => fb.map(b => (a, b)))
  }
  // course solution
  val zipListSemigroupalR: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](
        fa: List[A],
        fb: List[B]
    ): List[(A, B)] = fa zip fb
  }

  def main(args: Array[String]): Unit = {
    println(aTupledList)

    println(productWithMonads(Option(3), Option("a")))

    println(invalidsCombination)
    println(eitherCombination)

    println(zipListSemigroupal.product(List(1, 2, 3), List('a', 'b')))
    println(zipListSemigroupalR.product(List(1, 2, 3), List('a', 'b')))
  }
}
