package lists

import lists.model.{Item, Kind}

import scala.annotation.tailrec

class RemoveDuplicates {

  def rerank(kind: Kind, tail: List[Item]): List[Item] =
    tail match {
      case Nil => Nil
      case x :: Nil => List(x)
      case xs => xs.find(_.kind == kind).fold {
        xs
      } { item =>
        val (ys, zs) = xs.span(_ != item)
        item :: ys ::: (zs match {
          case Nil => List.empty[Item]
          case _ :: Nil => List.empty[Item]
          case _ :: zs => zs
        })

      }
    }

  def removeDuplicatesAndRerank(ls: List[Item]): List[Item] =
    ls match {
      case Nil => Nil
      case x :: Nil => List(x)
      case x :: xs if xs.exists(_.id == x.id) => {
        val itemToDelete = xs.find(_.id == x.id)
        itemToDelete.fold {
          x :: removeDuplicatesAndRerank(xs)
        } { item =>
          val (ys, zs) = xs.span(_ != item)
          (ys, zs) match {
            case (_, _ :: Nil) => x :: removeDuplicatesAndRerank(ys)
            case (ys, zs) => removeDuplicatesAndRerank(x :: ys ++ rerank(zs.head.kind, zs.tail))
          }
        }
      }
      case x :: xs => x :: removeDuplicatesAndRerank(xs)
    }

  @tailrec
  final def removeDuplicatesAndRerankTailRec(ls: List[Item], partialResult: List[Item] = Nil): List[Item] =
    ls match {
      case Nil => partialResult
      case x :: Nil => partialResult :+ x
      case x :: xs if xs.exists(_.id == x.id) => {
        val itemToDelete = xs.find(_.id == x.id)
        val (ys, zs) =
          if (itemToDelete.isEmpty) {
            (xs, partialResult :+ x)
          } else {
            val item = itemToDelete.get
            val (yss, zss) = xs.span(_ != item)
            (yss, zss) match {
              case (_, _ :: Nil) => (yss, partialResult :+ x)
              case (ysss, zsss) => (x :: ysss ++ rerank(zsss.head.kind, zsss.tail), partialResult)
            }
          }
        removeDuplicatesAndRerankTailRec(ys, zs)
      }
      case x :: xs => removeDuplicatesAndRerankTailRec(xs, partialResult :+ x)
    }


  //  def removeDuplicatesAndRerank(ls: List[Item]): List[Item] =
  //    ls.distinctBy(_.id)
  //  def removeDuplicatesAndRerank(ls: List[Item]): List[Item] =
  //    ls.distinct
}
