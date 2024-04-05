package recursion_schemes.nofixpoints

import recursion_schemes.nofixpoints.trees.TreeFFix.{cataFix, outF}

import scala.annotation.tailrec

object trees {

  sealed trait Tree
  case class Leaf(value: BigInt)           extends Tree
  case class Node(left: Tree, right: Tree) extends Tree

  object Tree {
    def foldTree[A](onLeaf: BigInt => A, onNode: (A, A) => A): Tree => A = {
      case Leaf(i)    => onLeaf(i)
      case Node(l, r) => onNode(foldTree(onLeaf, onNode)(l), foldTree(onLeaf, onNode)(r))
    }

    def sumTreeFold: Tree => BigInt = foldTree[BigInt](identity, _ + _)

    def countLeavesFold: Tree => BigInt = foldTree[BigInt](_ => 1, _ + _)
    def countLeavesRec: Tree => Int = {
      case Leaf(_)    => 1
      case Node(l, r) => countLeavesRec(l) + countLeavesRec(r)
    }
    def countLeavesTR(tree: Tree): Int = {
      @tailrec
      def countLeavesAccTR(ls: List[Tree], acc: Int): Int = ls match {
        case Nil                      => acc
        case Leaf(_) :: tail          => countLeavesAccTR(tail, acc + 1)
        case Node(Leaf(_), r) :: tail => countLeavesAccTR(r :: tail, acc + 1)
        case Node(l, Leaf(_)) :: tail => countLeavesAccTR(l :: tail, acc + 1)
        case Node(l, r) :: tail       => countLeavesAccTR(l :: r :: tail, acc)
      }

      countLeavesAccTR(List(tree), 0)
    }
  }

  sealed trait TreeF[T]
  case class LeafF[T](value: BigInt)     extends TreeF[T]
  case class NodeF[T](left: T, right: T) extends TreeF[T]

  object TreeF {
    def in: TreeF[Tree] => Tree = {
      case LeafF(v)    => Leaf(v)
      case NodeF(l, r) => Node(l, r)
    }
    def out: Tree => TreeF[Tree] = {
      case Leaf(v)    => LeafF(v)
      case Node(l, r) => NodeF(l, r)
    }

    trait Functor[F[_]] {
      def map[A, B](f: A => B): F[A] => F[B]
    }

    implicit val treeFFunctor: Functor[TreeF] = new Functor[TreeF] {
      override def map[A, B](f: A => B): TreeF[A] => TreeF[B] = {
        case LeafF(v)    => LeafF(v)
        case NodeF(l, r) => NodeF(f(l), f(r))
      }
    }

    def cata[F[_], R, A](algebra: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A =
      algebra(F.map(cata(algebra, out))(out(r)))
  }

  final case class Fix[F[_]](unfix: F[Fix[F]])

  object TreeFFix {
    def inF: TreeF[Fix[TreeF]] => Fix[TreeF]  = Fix(_)
    def outF: Fix[TreeF] => TreeF[Fix[TreeF]] = _.unfix

    trait Functor[F[_]] {
      def map[A, B](f: A => B): F[A] => F[B]
    }

    implicit val treeFFunctor: Functor[TreeF] = new Functor[TreeF] {
      override def map[A, B](f: A => B): TreeF[A] => TreeF[B] = {
        case LeafF(v)    => LeafF(v)
        case NodeF(l, r) => NodeF(f(l), f(r))
      }
    }

    def cataFix[F[_], R, A](algebra: F[A] => A, out: R => F[R])(r: R)(implicit F: Functor[F]): A =
      algebra(F.map(cataFix(algebra, out))(out(r)))
  }

}

import recursion_schemes.nofixpoints.trees._

object TreeData {
  val tree =
    Node(
      Node(
        Leaf(1),
        Node(
          Leaf(2),
          Leaf(3)
        )
      ),
      Node(
        Leaf(4),
        Node(
          Node(
            Leaf(5),
            Leaf(6)
          ),
          Leaf(7)
        )
      )
    )

  val treeFix: Fix[TreeF] = Fix(LeafF(1))

}

import recursion_schemes.nofixpoints.TreeData._

object TreeRunner extends App {
  println(
    s"Sum ${Tree.sumTreeFold(tree)}"
  )
  println(
    s"Leaves using fold ${Tree.countLeavesFold(tree)}"
  )
  println(
    s"Leaves using recursion ${Tree.countLeavesRec(tree)}"
  )
  println(
    s"Leaves using tail recursion ${Tree.countLeavesTR(tree)}"
  )

  ///////////////////////
  // Recursion Schemes //
  ///////////////////////
  import recursion_schemes.nofixpoints.trees.TreeF._
  def countLeavesAlgebra: TreeF[Int] => Int = {
    case LeafF(_)    => 1
    case NodeF(l, r) => l + r
  }

  def countLeavesRS: Tree => Int = cata(countLeavesAlgebra, out)

  println(
    s"Leaves using recursion schemes ${countLeavesRS(tree)}"
  )

  ////////////////////////////////
  // Recursion Schemes Fixpoint //
  ////////////////////////////////
  import recursion_schemes.nofixpoints.trees.TreeFFix.treeFFunctor
  def countLeavesRSFix: Fix[TreeF] => Int = cataFix(countLeavesAlgebra, outF)

  println(
    s"Leaves using recursion schemes fixpoint ${countLeavesRSFix(treeFix)}"
  )
}
