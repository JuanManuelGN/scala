package rock.cats

object Lesson10_State {

  /*
    S = State S => (S, A)
              ^     ^
            Before After
    A = Valor deseado despues de una computación
   */
  type MyState[S, A] = S => (S, A)

  import cats.data.State
  val countAndSay: State[Int, String] =
    State(currentCount => (currentCount + 1, s"Counted $currentCount"))
  val (eleven, counted10) = countAndSay.run(10).value
  // state = "iterative" computations

  // Java style
  var a = 10
  a += 1
  val firstComputation = s"Added 1 to 10 obtained $a"
  a *= 5
  val secondComputation = s"Multiplied with 5 obtained $a"

  // pure FP with states
  val firstComputationFP  = State((s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}"))
  val secondComputationFP = State((s: Int) => (s * 5, s"Multiplied with 5 obtained ${s * 5}"))
  val compositeTransformation = firstComputationFP.flatMap { firstResult =>
    secondComputationFP.map(secondResult => (firstResult, secondResult))
  }
  val compositeTransformationfor = for {
    firstResult  <- firstComputationFP
    secondResult <- secondComputationFP
  } yield (firstResult, secondResult)

  val f1 = (s: Int) => (s + 1, s"Added 1 to 10, obtained ${s + 1}")
  val f2 = (s: Int) => (s * 5, s"Multiplied with 5 obtained ${s * 5}")
  val compositeResult = f1.andThen { case (newState, firstResult) =>
    (firstResult, f2(newState))
  } // .andThen ... Si volvermos a usar la composición debemos decomponer el case con 3 elementos, por ello el uso de State.
//    .andThen {
//      case (firstR, (newState, secondR)) => ???
//    }

  // TODO: an online store
  case class ShoppingCart(items: List[String], total: Double)
  object ShoppingCart {
    def empty: ShoppingCart = ShoppingCart(List.empty[String], 0.0)
  }
  def addToCard(item: String, price: Double): State[ShoppingCart, Double] =
    State(sc => (ShoppingCart(sc.items :+ item, sc.total + price), sc.total + price))

  val juanCart: State[ShoppingCart, Double] = for {
    _     <- addToCard("pen", 1.4)
    _     <- addToCard("tablet", 400.0)
    total <- addToCard("charger", 23.6)
  } yield total

  // TODO:
  /*
    No cambia el estado (A) pero si aplica la función f(A) = B
    returns a State data structure that, when run, will not change the state but will issue the value f(a)
   */
  def inspect[A, B](f: A => B): State[A, B] = State((a: A) => (a, f(a)))
  /*
    returns a State data structure that, when run, returns the value of that state and make no changes
   */
  def get[A]: State[A, A] = State((a: A) => (a, a))
  /*
    returns a State data structure that, when run, returns a Unit and sets the state to that value
   */
  def set[A](value: A): State[A, Unit] = State((_: A) => (value, ()))
  /*
    returns a State data structure that, when run, will return Unit and sets the state to f(state)
   */
  def modify[A](f: A => A): State[A, Unit] = State((s: A) => (f(s), ()))

  // todos estos métodos están implementados en el companion de State
  import cats.data.State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 10)
    b <- get[Int]
    _ <- modify[Int](_ + 43)
    c <- inspect[Int, Int](_ * 2)
  } yield (a, b, c)

  def main(args: Array[String]): Unit = {
    println(firstComputationFP.run(4).value)

    println(
      compositeTransformation.run(10).value
    ) // (55,(Added 1 to 10, obtained 11,Multiplied with 5 obtained 275)) ha combinado el resultado de los dos string en uno

    println(compositeResult(10))

    println(juanCart.run(ShoppingCart.empty).value)

    println(inspect[Int, Int](_ * 5).run(4).value)
    println(get[String].run("Hello").value)
    println(set("I'm a State").run("State 0").value)
    println(modify[Int](_ * 2).run(2).value)

    println(program.run(3).value)

  }
}
