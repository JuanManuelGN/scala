package rock.cats

import scala.util.Try

object Lesson03Functors {

  val aModifiedList   = List(1, 2, 3).map(_ + 1) // List(2,3,4)
  val aModifiedOption = Option(3).map(_ + 1)     // Some(4)
  val aModifiedTry    = Try(42).map(_ + 1)       // Success(43)

  // Definición de Functor
  trait MyFunctor[F[_]] {
    def map[A, B](initialValue: F[A])(f: A => B): F[B]
  }

  import cats.Functor
  import cats.instances.list._
  val listFunctor        = Functor[List]
  val incrementedNumbers = listFunctor.map(List(1, 2, 3))(_ + 1) // List(2,3,4)

  import cats.instances.option._
  val optionFunctor     = Functor[Option]
  val incrementedOption = optionFunctor.map(Option(3))(_ + 1) // Some(4)

  import cats.instances.try_._
  val incrementedTry = Functor[Try].map(Try(4))(_ + 1) // Success(5)

  // Generalizing API
  def do10xList(list: List[Int]): List[Int]         = list.map(_ * 10)
  def do10xOption(option: Option[Int]): Option[Int] = option.map(_ * 10)
  def do10xTry(attemp: Try[Int]): Try[Int]          = attemp.map(_ * 10)

  // generalize
  def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] =
    functor.map(container)(_ * 10)

  // Exercise 1: define your own functor for a binay tree
  trait Tree[+T]
  object Tree {
    def leaf[T](value: T): Tree[T]                                  = Leaf(value)
    def branch[T](value: T, left: Tree[T], right: Tree[T]): Tree[T] = Branch(value, left, right)
  }
  case class Leaf[+T](value: T)                           extends Tree[T]
  case class Branch[+T](value: T, l: Tree[T], r: Tree[T]) extends Tree[T]

  implicit object treeFunctor extends Functor[Tree] {
    override def map[A, B](initialValue: Tree[A])(f: A => B): Tree[B] = initialValue match {
      case Leaf(v)         => Leaf(f(v))
      case Branch(v, l, r) => Branch(f(v), map(l)(f), map(r)(f))
    }
  }

  // extension method - map
  import cats.syntax.functor._
  val tree: Tree[Int] = Tree.branch(40, Tree.branch(5, Tree.leaf(10), Tree.leaf(30)), Tree.leaf(20))
  val incrementedTree = tree.map(_ + 1) // funciona porque tenemos una instncia implicita de Functor

  // Exercise 2: write a shorted do10x using extension methods
  // definición original:
  // def do10x[F[_]](container: F[Int])(implicit functor: Functor[F]): F[Int] = functor.map(container)(_ * 10)
  def do10xShorter[F[Int]: Functor](fa: F[Int]): F[Int] = Functor[F].map(fa)(_ * 10)
  // esta solución es la que se da en el curso
  def do10xShorterRock[F[_]: Functor](fa: F[Int]): F[Int] = fa.map(_ * 10)

  def main(args: Array[String]): Unit = {
    println(do10x(List(1, 2, 3)))
    println(do10x(Option(3)))
    println(do10x(Try(23)))

    val treeInt = Branch(10, Branch(4, Leaf(1), Leaf(3)), Leaf(5))
    println(
      do10x[Tree](
        treeInt
      ) // tenemos que tipar la llamada a la función porque si no el compilador no reconoce el implicit, para evitar esto podemos usar smart constructors
    )

    val treeIntSmart = Tree.branch(10, Tree.branch(4, Tree.leaf(1), Tree.leaf(3)), Tree.leaf(5))
    println(do10x(treeIntSmart)) // ahora no es necesario tipar la función

    println(do10xShorter(Option(10)))     // Some(100)
    println(do10xShorterRock(Option(10))) // Some(100)

    println(do10xShorter(treeIntSmart))
    println(do10xShorterRock(treeIntSmart))
  }
}
