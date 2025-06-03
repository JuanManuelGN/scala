package rock.cats

/*
  Wrap functions returning F[_] instances
  map, flatMap, apply, andThen, traverse ...
  Used for
    - function composition when they return F[_] types
    - dependency injection - Reader!
 */
object Lesson19_Kleisli {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int]    = x => Some(x * 3)

  // val func3 = func2 andThen func1 // compile fails

  // func3 = func2 andThen func1
  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int    = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli       // es un wrapper sobre una función
  import cats.instances.option._ // FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1) // from Int to Option[String]
  val func2K: Kleisli[Option, Int, Int]    = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain    = func2K.flatMap(_ => func1K)

  // todo. Dependency injection
  import cats.Id
  type InterestingKleisli[Id, A, B] // wrapper over A => Id[B]
  // hint
  val times2   = Kleisli[Id, Int, Int](x => x * 2)
  val plus4    = Kleisli[Id, Int, Int](y => y + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4
  
  // InterestingKleisli === Reader
  import cats.data.Reader
  val times2R  = Reader[Int, Int](x => x * 2)
  val plus4R    = Reader[Int, Int](y => y + 4)
  val composedForR = for {
    t2 <- times2R
    p4 <- plus4R
  } yield t2 + p4
  

  def main(args: Array[String]): Unit = {
    println(composedFor(3))
    println(composedForR(3)) // same result
  }
}
