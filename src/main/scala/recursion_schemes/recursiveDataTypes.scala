package recursion_schemes

/**
  * https://www.youtube.com/watch?v=XZ9nPZbaYfE
  */
object recursiveDataTypes {

  sealed trait List
  case object Nil extends List
  case class Cons(head: BigInt, tail: List) extends List

  def multiply: List => BigInt = {
    case Nil => 1
    case Cons(i, t) => i * multiply(t)
  }

  def length: List => BigInt = {
    case Nil => 0
    case Cons(i, t) => 1 + length(t)
  }

  def foldList[A](onNil: A, onCons: (BigInt, A) => A): List => A = {
    case Nil => onNil
    case Cons(i, t) => onCons(i, foldList(onNil, onCons)(t))
  }

  def multiplyFold: List => BigInt = foldList[BigInt](1, _ * _)

  def lengthFold: List => BigInt = foldList[BigInt](0, (_, len) => len + 1)

  sealed trait Tree
  case class Leaf(value: BigInt) extends Tree
  case class Node(left: Tree, right: Tree) extends Tree

  def foldTree[A](onLeaf: BigInt => A, onNode: (A, A) => A): Tree => A = {
    case Leaf(i) => onLeaf(i)
    case Node(l, r) => onNode(foldTree(onLeaf, onNode)(l), foldTree(onLeaf, onNode)(r))
  }

  def sum: Tree => BigInt = foldTree[BigInt](identity, _ + _)

  def countLeaves: Tree => BigInt = foldTree[BigInt](_ => 1, _ + _)

  sealed trait ListF[A]
  case class NilF[A]() extends ListF[A]
  case class ConsF[A](head: BigInt, a: A) extends ListF[A]

  def in: ListF[List] => List = {
    case NilF() => Nil
    case ConsF(h, t) => Cons(h, t)
  }

  def out: List => ListF[List] = {
    case Nil => NilF()
    case Cons(h, t) => ConsF(h, t)
  }

  trait Functor[F[_]] {
    def map[A, B](f: A => B): F[A] => F[B]
  }

  implicit val listFFunctor: Functor[ListF] = new Functor[ListF] {
    override def map[A, B](f: A => B): ListF[A] => ListF[B] = {
      case NilF() => NilF()
      case ConsF(h, t) => ConsF(h, f(t))
    }
  }

  type FAlgebra[F[_], A] = F[A] => A

  def inF: FAlgebra[ListF, List] = {
    case NilF() => Nil
    case ConsF(h, t) => Cons(h, t)
  }

  def multiplyAlgebra: FAlgebra[ListF, BigInt] = {
    case NilF() => 1
    case ConsF(h, t) => h * t
  }

  def cata[F[_], R, A](algebra: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A =
    algebra(F.map(cata(algebra, out))(out(r)))

  def multiplyList: List => BigInt = cata(multiplyAlgebra, out)

  def range: BigInt => List = // ana
    n => if (n > 0) Cons(n, range(n - 1)) else Nil

  type Coalgebra[F[_], A] = A => F[A]

  def ana[F[_], R, A](coalgebra: Coalgebra[F, A], in: F[R] => R)(a: A)(implicit F: Functor[F]): R =
    in(F.map(ana(coalgebra, in))(coalgebra(a)))

  def rangeCoalgebra: Coalgebra[ListF, BigInt] =
    n => if (n > 0) ConsF(n, n - 1) else NilF()

  def rangeF: BigInt => List = ana(rangeCoalgebra, inF)

  def factorial: BigInt => BigInt = n => // hylomorphism
    if (n > 0) n * factorial(n - 1) else 1

  def hylo[F[_], A, B](coalgebra: A => F[A], algebra: F[B] => B)(a: A)(implicit F: Functor[F]): B =
    algebra(F.map(hylo(coalgebra, algebra))(coalgebra(a)))

  def factorialF: BigInt => BigInt = hylo(rangeCoalgebra, multiplyAlgebra)


}


import recursiveDataTypes._

object Runner2 extends App {
  val ls = Cons(3, Cons(2, Cons(1, Nil)))

  println(
    multiply(ls)
  )

  println(
    length(ls)
  )

  println(
    multiplyFold(ls)
  )

  println(
    lengthFold(ls)
  )


  val lsF = ConsF(1, ConsF(2, ConsF(3, NilF)))

  println(
    s"multiplicar lista usando cata: ${multiplyList(ls)}"
  )

  println(
    s"unfold using ana ${rangeF(5)}"
  )

  println(
    s"unfold and fold to calculate factorial using hylo ${factorialF(100)}"
  )

}
