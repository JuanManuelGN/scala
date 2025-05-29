package rock.cats

import cats.Applicative
import cats.Foldable
import cats.Functor
import cats.Monad

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

/*
  Higher-kinded TC with insede-out functions
  Useful for
  - turning nested dat structures insede out
  - general data combination APIs
  
  Cats Type Class hierarchy

    Semigroup   Foldable    Functor  Semigroupal
        ^            ^       ^   ^   ^
      Monoid         Traverse    Apply
                                 ^   ^
                            FlatMap Applicative
                                 ^   ^    ^
                                 Monad   ApplicativeError
                                    ^     ^
                                   MonadError
 */
object Lesson18_Traversing {

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val servers: List[String] = List(
    "server-ci.rockthejvm.com",
    "server-staging.rockthejvm.com",
    "prod.rockthejvm.com"
  )
  def getBandwidth(hostname: String): Future[Int] = Future(hostname.length * 80)

  /*
    we have a List[String]
    String => Future[Int]
    we want a Future[List[Int]]
   */
  val allBandwwidths: Future[List[Int]] = servers.foldLeft(Future(List.empty[Int])) {
    (acc, hostname) =>
      val bandFuture = getBandwidth(hostname)
      for {
        accBandwidths <- acc
        band          <- bandFuture
      } yield accBandwidths :+ band
  }
  //  ^^^^
  val allBandwidthsTraverse: Future[List[Int]] =
    Future.traverse(servers)(getBandwidth) // Future[List[Int]]

  val allBandwidthsSequence: Future[List[Int]] = Future.sequence(servers.map(getBandwidth))

  // todo
  import cats.syntax.applicative._ // pure
  import cats.syntax.flatMap._     // flatMap
  import cats.syntax.functor._     // map
  def listTraverse[F[_]: Monad, A, B](ls: List[A])(f: A => F[B]): F[List[B]] =
    ls.foldLeft(List.empty[B].pure[F]) { (acc, x) =>
      val fx = f(x)
      for {
        a <- acc
        u <- fx
      } yield a :+ u
    }
  import cats.syntax.apply._ // mapN
  def listTraverseWeaker[F[_]: Applicative, A, B](ls: List[A])(f: A => F[B]): F[List[B]] =
    ls.foldLeft(List.empty[B].pure[F]) { (acc, x) =>
      val fx = f(x)
      (acc, fx).mapN(_ :+ _)
    }

  // todo
  def listSequence[F[_]: Applicative, A](ls: List[F[A]]): F[List[A]] =
    ls.foldLeft(List.empty[A].pure[F])((acc, a) => (acc, a).mapN(_ :+ _))
  def listSequenceRock[F[_]: Applicative, A](ls: List[F[A]]): F[List[A]] =
    listTraverseWeaker(ls)(identity)

  // todo Qué devuelven las siguientes expresiones?
  import cats.instances.vector._
  val allPairs = listSequenceRock(
    List(Vector(1, 2), Vector(3, 4))
  ) // Vector[List[Int]] - all the possible 2-tuples
  val allTriples = listSequenceRock(
    List(Vector(1, 2), Vector(3, 4), Vector(5, 6))
  ) // Vector[List[Int]] - - all the possible 3-tuples

  import cats.syntax.option._
  def filterAsOption(ls: List[Int])(p: Int => Boolean): Option[List[Int]] =
    listTraverseWeaker[Option, Int, Int](ls)(n => Some(n).filter(p))
  // todo - what's the result of
  val allTrue = filterAsOption(List(2, 4, 6))(_ % 2 == 0) // Some(List(2,4,6))
  val someTrue = filterAsOption(List(1, 2, 3))(
    _ % 2 == 0
  ) // None porque combinamos Some(2) con None y None de aplicar el filtro a 1 y 3

  import cats.data.Validated
  import cats.instances.list._ // Semigroup[List] => Applicative[ErrorsOr]
  type ErrorsOr[T] = Validated[List[String], T]
  def filterAsValidated(ls: List[Int])(p: Int => Boolean): ErrorsOr[List[Int]] =
    listTraverseWeaker[ErrorsOr, Int, Int](ls) { n =>
      if (p(n)) Validated.valid(n)
      else Validated.invalid(List(s"predicate for $n failed"))
    }

  // todo - what's the result of
  val allTrueValidated = filterAsValidated(List(2, 4, 6))(_ % 2 == 0) // Valid(List(2,4,6)
  val someTrueValidated = filterAsValidated(List(1, 2, 3))(
    _ % 2 == 0
  ) // Invalid(List("predicate for 1 failed", "predicate for 3 failed"))

  trait MyTraverse[L[_]] extends Foldable[L] with Functor[L] {
    def traverse[F[_]: Applicative, A, B](container: L[A])(f: A => F[B]): F[L[B]]
    def sequence[F[_]: Applicative, A](container: L[F[A]]): F[L[A]] = traverse(container)(identity)

    // todo - implement in terms of traverse and/or sequence methods
    // hint
    type Identity[T] = T
    // import cats.Id es igual que usar Identity
    def map[A, B](wa: L[A])(f: A => B): L[B] = {
      traverse[Identity, A, B](wa)(f.pure[Identity])
//      traverse[Id, A, B](wa)(f)
    }
  }

  import cats.Traverse
  import cats.instances.future._ // Applicative[Future]
  val allBandwidthCats = Traverse[List].traverse(servers)(getBandwidth)

  // extension methods
  import cats.syntax.traverse._ // sequence + traverse
  val allBandwidthCats2 = servers.traverse(getBandwidth)

  def main(args: Array[String]): Unit = {
    val ls = List(1, 2, 3)
    println(listTraverse(ls)((x: Int) => Option(x)))
    println(listTraverseWeaker(ls)((x: Int) => Option(x)))

    val lsOption = List(Option(1), Option(2))
    println(listSequence(lsOption))
    println(listSequenceRock(lsOption))

    println(allPairs)
    println(allTriples)

    println(allTrue)
    println(someTrue)

    import cats.data.Validated
    println(Validated.valid(2).combine(Validated.invalid("Error")))

    println(allTrueValidated)
    println(someTrueValidated)
  }

}
