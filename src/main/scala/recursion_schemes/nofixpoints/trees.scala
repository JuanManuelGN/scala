package recursion_schemes.nofixpoints

object trees {

  sealed trait Tree
  case class Leaf(value: BigInt)           extends Tree
  case class Node(left: Tree, right: Tree) extends Tree

  object Tree {
    def foldTree[A](onLeaf: BigInt => A, onNode: (A, A) => A): Tree => A = {
      case Leaf(i)    => onLeaf(i)
      case Node(l, r) => onNode(foldTree(onLeaf, onNode)(l), foldTree(onLeaf, onNode)(r))
    }

    def sum: Tree => BigInt = foldTree[BigInt](identity, _ + _)

    def countLeaves: Tree => BigInt = foldTree[BigInt](_ => 1, _ + _)
  }

}
