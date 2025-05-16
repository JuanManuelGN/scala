package rock.cats

import java.util.concurrent.Executors

import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/**
 * Monads
 * Higher-Kinded type class that provides
 *  - a pure method to wrap a normal value into a monadic value
 *  - a flatMap method to transform monadic values in sequence
 * Can implement map in terms of pure + flatMap
 *  - Monads extends Functors
 * Extension methods are in other packages
 * map + flatMap = for-comprehensions
 * Use cases: sequential transformations
 *  - list combinations
 *  - option transformations
 *  - asynchronous chained computations
 *  - dependent computations
 */
object Lesson04_Monads {

  val numberList = List(1, 2, 3)
  val charList   = List('a', 'b', 'c', 'd')

  // todo: how do you create all combinations of (numbers, char)?
  val combinations = numberList.flatMap(n => charList.map(c => (n, c)))
  val combinationsFor = for {
    n <- numberList
    c <- charList
  } yield (n, c)

  // options
  val numberOpt = Option(2)
  val charOpt   = Option('d')
  // todo: same with options
  val combinationOption = numberOpt.flatMap(o => charOpt.map(c => (o, c)))
  val combinationOptionFor = for {
    n <- numberOpt
    c <- charOpt
  } yield (n, c)

  // futures
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(8))
  val numberFuture                  = Future(4)
  val charFuture                    = Future('c')
  // todo: same for future
  val combinationFuture = numberFuture.flatMap(o => charFuture.map(c => (o, c)))
  val combinationFutureFor = for {
    n <- numberFuture
    c <- charFuture
  } yield (n, c)

  /*
      Pattern
      - wrapping a value into a monadic value
      - the flatMap function. Función de transformación aplicable a varios tipos de datos. Secuenciar operaciones

      Monads
   */
  trait MyMonad[M[_]] {
    def pure[A](value: A): M[A]
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
    // todo implement this
    def map[A, B](ma: M[A])(f: A => B): M[B] = flatMap(ma)(a => pure[B](f(a)))
  }

  // cats monads
  import cats.Monad
  import cats.instances.option._ // implicit Monad[Option]
  val optionMonad        = Monad[Option]
  val anOption           = optionMonad.pure(4) // Option(4) == Some(4)
  val aTransformedOption = optionMonad.flatMap(anOption)(x => if (x % 3 == 0) Some(x + 1) else None)

  import cats.instances.list._
  val listMonad        = Monad[List]
  val aList            = listMonad.pure(4)                             // List(4)
  val aTransformedList = listMonad.flatMap(aList)(x => List(x, x + 1)) // List(5, 6)

  // todo: use a Monad[Future]
  import cats.instances.future._
  val futureMonad        = Monad[Future]
  val aFuture            = futureMonad.pure(5)
  val aTransformedFuture = futureMonad.flatMap(aFuture)(x => Future(x + 5))

  // specialized API
  def getPairsList(numbers: List[Int], chars: List[Char]): List[(Int, Char)] =
    numbers.flatMap(n => chars.map(c => (n, c)))
  // Si queremos cambiar el tipo de List[Int] a Option[Int] tenemos que copiar el método creando uno
  // nuevo con los nuevos tipos
  def getPairsOption(number: Option[Int], char: Option[Char]): Option[(Int, Char)] =
    number.flatMap(n => char.map(c => (n, c)))
  // ...

  // generalize
  def getPairs[M[_], A, B, R](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] =
    monad.flatMap(ma)(a => monad.map(mb)(b => (a, b)))

  // extension methods
  import cats.syntax.applicative._ // pure is here
  val oneOption = 1.pure[Option] // implicit Monad[Option] will be use => Some(1)
  val oneList   = 1.pure[List]   // List(1)

  import cats.syntax.flatMap._ // flatMap is here
  val transformedOption = oneOption.flatMap(x => if (x % 3 == 0) (x + 1).pure[Option] else None)

  // todo: implement the map method in MyMonad
  // Monads extends Functors
  val oneOptionMapped = Monad[Option].map(Option(2))(_ + 1) // Some(3)
  import cats.syntax.functor._ // map is here
  val oneOptionMapped2 = oneOption.map(_ + 2)

  // for-comprehension
  val composedOption = for {
    one <- 1.pure[Option]
    two <- 2.pure[Option]
  } yield one + two // Some(3)

  // todo: implement a shorter version of getPairs using for-comprehension
  def getPairsFor[M[_], A, B](ma: M[A], mb: M[B])(implicit monad: Monad[M]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b) // same as ma.flatMap(a => mb.map(b => (a, b)))

  // se puede definir así, de forma mas corta
  def getPairsForShorter[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = for {
    a <- ma
    b <- mb
  } yield (a, b) // same as ma.flatMap(a => mb.map(b => (a, b)))
  
  // next video https://courses.rockthejvm.com/courses/1107955/lectures/23728907

  def main(arg: Array[String]): Unit = {
//    println(getPairs(numberList, charList))
//    println(getPairs(numberOpt, charOpt))
//    getPairs(numberFuture, charFuture).foreach(println)
    getPairsFor(numberList, charList).foreach(println)

  }
}
