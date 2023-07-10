package lists

import lists.model.{Item, Kind}

class RemoveDuplicates {

  def rerank(kind: Kind, tail: List[Item]): List[Item] =
    tail match {
      case Nil => Nil
      case x :: Nil => List(x)
      case xs => xs.find(_.kind == kind).fold{
        xs
      }{ item =>
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
        itemToDelete.fold{
          x :: removeDuplicatesAndRerank(xs)
        }{ item =>
          val (ys, zs) = xs.span(_ != item)
          (ys, zs) match {
            case (_, _ :: Nil) => x :: removeDuplicatesAndRerank(ys)
            case (ys, zs) => removeDuplicatesAndRerank(x :: ys ++ rerank(zs.head.kind, zs.tail))
          }
        }
      }
      case x :: xs => x :: removeDuplicatesAndRerank(xs)
    }


//  def removeDuplicatesAndRerank(ls: List[Item]): List[Item] =
//    ls.distinctBy(_.id)
//  def removeDuplicatesAndRerank(ls: List[Item]): List[Item] =
//    ls.distinct

}
