package recursion_schemes.nofixpoints

import scala.annotation.tailrec

/**
  * Implementación de listas usando fold y recursión
  */
object list {

  sealed trait List
  case object Nil                           extends List
  case class Cons(head: BigInt, tail: List) extends List

  object List {
    def multiply: List => BigInt = {
      case Nil        => 1
      case Cons(i, t) => i * multiply(t)
    }

    @tailrec
    def multiplyTR(acc: BigInt = 1, ls: List): BigInt = ls match {
      case Nil        => acc
      case Cons(i, t) => multiplyTR(i * acc, t)
    }

    def length: List => BigInt = {
      case Nil        => 0
      case Cons(i, t) => 1 + length(t)
    }

    def foldList[A](onNil: A, onCons: (BigInt, A) => A): List => A = {
      case Nil        => onNil
      case Cons(i, t) => onCons(i, foldList(onNil, onCons)(t))
    }

    def multiplyFold: List => BigInt = foldList[BigInt](1, _ * _)

    def lengthFold: List => BigInt = foldList[BigInt](0, (_, len) => len + 1)
  }

}
