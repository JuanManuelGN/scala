package adt

import adt.treealg.{GeneralNode, Leaf, Predicate, TreeIntImpl, TreeListIntImpl}

object treealg {

  /**
    * AST para representar de forma genérica la evaluación de condiciones en una sobre una estructura
    * de datos, por ejemplo se quiere saber si una seríe de predicados combiandos en un árbol con operaciones
    * lógicas son ciertos o no:
    *                           and
    *                       and    (x = 4)
    *                (x > 3)   (x < 5)
    * @tparam A
    */
  sealed trait Tree[+A]
  case class Leaf[A](p: Predicate[A]) extends Tree[A]
  case class UnaryNode[A](operator: Boolean => Boolean, branch: Tree[A]) extends Tree[A]
  case class GeneralNode[A](operator: (Boolean, Boolean) => Boolean, branches: Tree[A]*) extends Tree[A]
  case class Predicate[A](f: A => Boolean)

  object TreeIntImpl {
    def eval(t: Tree[Int], x: Int): Boolean =
      t match {
        case Leaf(p) => p.f(x)
        case UnaryNode(f, b) => f(eval(b, x))
        case GeneralNode(f, bs @ _*) => bs.map(eval(_, x)).reduceLeft(f)
      }
  }

  object TreeListIntImpl {
    def eval(t: Tree[List[Int]], xs: List[Int]): Boolean =
      t match {
        case Leaf(p) => p.f(xs)
        case UnaryNode(f, b) => f(eval(b, xs))
        case GeneralNode(f, bs@_*) => bs.map(eval(_, xs)).reduceLeft(f)
      }
  }
}

object Ex extends App {
  val intTree =
    GeneralNode[Int](_ && _,
        Leaf[Int](Predicate(_ > 3)),
        Leaf(Predicate[Int](_ < 5))
    )
  println(intTree)
  println(TreeIntImpl.eval(intTree, 4))

  val listIntTree =
    GeneralNode[List[Int]](_ && _,
      Leaf(Predicate[List[Int]](ls => (ls.sum/ls.size) > 3)),
      Leaf(Predicate[List[Int]](ls => (ls.sum/ls.size) < 5))
    )
  println(listIntTree)
  println(TreeListIntImpl.eval(listIntTree, 4 :: 5 :: Nil))
}
