package rock.cats

import cats.Eval
import cats.Monoid

/*
  - Higher-kinded type with folding methods
  - Folding right is stack-safe, regardless of container using Eval
  
  Cats Type Class hierarchy

    Semigroup   Foldable    Functor  Semigroupal
        ^                        ^   ^
      Monoid                     Apply
                                 ^   ^
                            FlatMap Applicative
                                 ^   ^    ^
                                 Monad   ApplicativeError
                                    ^     ^
                                   MonadError 
 */
object Lesson17_Folding {

  // TODO implement all in terms of foldLeft and foldRight
  object ListExercise {
    def map[A, B](list: List[A])(f: A => B): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc :+ f(a))
    def mapFR[A, B](list: List[A])(f: A => B): List[B] =
      list.foldRight(List.empty[B])((a, acc) => f(a) +: acc)

    def flatMapRock[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc.foldRight(f(a))(_ :: _))
    def flatMap[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldLeft(List.empty[B])((acc, a) => acc ++ f(a))
    def flatMapFR[A, B](list: List[A])(f: A => List[B]): List[B] =
      list.foldRight(List.empty[B])((a, acc) => f(a) ++ acc)
    def filter[A](list: List[A])(p: A => Boolean): List[A] =
      list.foldLeft(List.empty[A])((acc, a) => if (p(a)) acc :+ a else acc)
    def filterFR[A](list: List[A])(p: A => Boolean): List[A] =
      list.foldRight(List.empty[A])((a, acc) => if (p(a)) a :: acc else acc)
    def combineAll[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldLeft(monoid.empty)((b, a) => monoid.combine(a, b))
    def combineAllFR[A](list: List[A])(implicit monoid: Monoid[A]): A =
      list.foldRight(monoid.empty)((b, a) => monoid.combine(a, b))
  }

  import cats.Foldable
  import cats.instances.list._ // implicit Foldable[List]
  val sum = Foldable[List].foldLeft(List(1, 2, 3), 0)(_ + _) // 6

  import cats.instances.option._ // implicit Foldable[Option]
  val sumOption = Foldable[Option].foldLeft(Option(2), 30)(_ + _) // 32

  /*
    Usando Eval obtenemos una recursión Stack-Safe independientemente del contenedor que usemos.
   */
  val sumRight: Eval[Int] = Foldable[List].foldRight(List(1, 2, 3), Eval.now(0)) { (num, eval) =>
    eval.map(_ + num)
  }

  import cats.instances.int._ // Monoid[Int]
  val anotherSum = Foldable[List].combineAll(List(1, 2, 3))

  import cats.instances.string._
  val mappedConcat = Foldable[List].foldMap(List(1, 2, 3))(_.toString)

  // nesting
  import cats.instances.vector._
  val instNested = List(Vector(1, 2, 3), Vector(4, 5, 6))
  (Foldable[List] compose Foldable[Vector]).combineAll(instNested)
  
  // extension methods
  import cats.syntax.foldable._
  val sum3 = List(1,2,3).combineAll // require Foldable[List] and Monoid[Int]
  val mappedConcat2 = List(1,2,3).foldMap(_.toString)

  def main(args: Array[String]): Unit = {
    val ls = List(1, 2, 3, 4)
    val f  = (x: Int) => x + 1
    val g  = (x: Int) => List(x + 1)
    import ListExercise._
    println("--map--")
    println(map(ls)(f))
    println(mapFR(ls)(f))
    println("--flatMap--")
    println(flatMap(ls)(g))
    println(flatMapFR(ls)(g))
    println(flatMapRock(ls)(x => (1 to x).toList))
    println("--filter--")
    val p = (x: Int) => x % 2 == 0
    println(filter(ls)(p))
    println(filterFR(ls)(p))
    println("--combineAll--")
    // import cats.instances.int._ // Monoid[Int], con esto no es necesario especificar el parámetro --> combineAll(ls)
    println(combineAll(ls)(Monoid[Int]))
    println(combineAllFR(ls)(Monoid[Int]))
  }

}
