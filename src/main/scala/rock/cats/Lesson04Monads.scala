package rock.cats
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Lesson04Monads {

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

  def main(arg: Array[String]): Unit = {
    println(getPairs(numberList, charList))
    println(getPairs(numberOpt, charOpt))
    getPairs(numberFuture, charFuture).foreach(println)

  }
}
