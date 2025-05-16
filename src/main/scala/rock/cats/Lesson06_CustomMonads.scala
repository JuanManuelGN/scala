package rock.cats

import scala.annotation.tailrec

object Lesson06_CustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A]                                   = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None           => None
      case Some(Left(v))  => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO: define a Monad instance for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42
  implicit object IdentityMonad extends Monad[Identity] {
    override def pure[A](x: A): Identity[A]                                       = x
    override def flatMap[A, B](wa: Identity[A])(f: A => Identity[B]): Identity[B] = f(wa)
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v)  => tailRecM(v)(f)
      case Right(b) => b
    }
  }

  sealed trait Tree[+A]
  final case class Leaf[+A](value: A)                        extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO: define a Monad instance for the Tree type
  implicit object TreeMonad extends Monad[Tree] {
    override def pure[A](x: A): Tree[A] = Leaf(x)
    override def flatMap[A, B](ta: Tree[A])(f: A => Tree[B]): Tree[B] = ta match {
      case Leaf(value)         => f(value)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }
    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] =
        t match {
          case Leaf(Right(v)) => Leaf(v)
          case Leaf(Left(la)) => stackRec(f(la))
          case Branch(l, r)   => Branch(stackRec(l), stackRec(r))
        }

      /* Implementation recursiva de cola
        ¿Para qué sirve? Para escribir bucles o recursiones dentro de un F[_] de forma stack-safe.
        ¿Cuándo lo usas? Cuando necesitas repetir un paso monádico hasta llegar a un resultado,
        sin usar flatMap recursivo que pueda desbordar.
       */
      @tailrec
      def tailRec(
          todo: List[Tree[Either[A, B]]],
          expanded: Set[Tree[Either[A, B]]],
          done: List[Tree[B]] // accumulator
      ): Tree[B] = {
        if (todo.isEmpty) done.head
        else
          todo.head match {
            case Leaf(Left(v)) =>
              println(s"tr([f($v)${todo.tail}], $expanded, $done)")
              tailRec(f(v) :: todo.tail, expanded, done)
            case Leaf(Right(b)) =>
              println(s"tr(${todo.tail}, $expanded, Leaf($b)$done)")
              tailRec(todo.tail, expanded, Leaf(b) :: done)
            case node @ Branch(l, r) =>
              if (!expanded.contains(node)) {
                println(s"tr($r$l$todo, $expanded + $node, $done)")
                tailRec(r :: l :: todo, expanded + node, done)
              } else {
                val newLeft   = done.head
                val newRight  = done.tail.head
                val newBranch = Branch(newLeft, newRight)
                println(s"tr(${todo.tail}, $expanded, $newBranch ${done.drop(2)}))")
                tailRec(todo.tail, expanded, newBranch :: done.drop(2))
              }
          }
      }

      stackRec(f(a))
//      tailRec(List(f(a)), Set(), List())
    }

  }
  def factorial(n: Long): Option[Long] = Monad[Option].tailRecM((n, 1L)) {
    case (0L, acc)          => Some(Right(acc))
    case (k, acc) if k > 0L => Some(Left((k - 1L, acc * k)))
    case _                  => None
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree     = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(changedTree)
    val f = (x: Int) =>
      if (x < 20) Leaf(Right(x))
      else if (x > 25) Leaf(Left(x - 1))
      else Branch(Leaf(Left(x - 1)), Leaf(Left(x - 2)))
    println(TreeMonad.tailRecM(27)(f))
    println(factorial(5))
  }
}
