package rock.cats

/** Monids: Es un conjunto 𝑀 con una operación binaria es un monoide si cumple que la operación es
  * asociativa y tiene elemento neutro
  */
object Lesson02Monoids {

  import cats.Semigroup
  import cats.instances.int._
  import cats.syntax.semigroup._ // for the |+| extension method

  val numbers = (1 to 1000).toList
  // |+| is associative
  val sumLeft  = numbers.foldLeft(0)(_ |+| _)
  val sumRight = numbers.foldRight(0)(_ |+| _)

  // define a general API
  // def combineFold[T](ls: List[T])(implicit semigroup: Semigroup[T]): T = {
  // Con un semigroup es imposible resolver este problema porque no hay elemento neutro y
  // se necesita para el foldLeft siguiente:
  // ls.foldLeft(/*elemento neutro*/)(_ |+| _)
  // Para esto necesitamos un Monoid, que es Un conjunto 𝑀 con una operación binaria es un monoide
  // si cumple que la operación es asociativa y tiene elemento neutro
  import cats.Monoid
  val intMonoid  = Monoid[Int]
  val combineInt = intMonoid.combine(2, 4) // 6
  val zero       = intMonoid.empty         // 0 for Int

  import cats.instances.string._ // bring the implicit Monoid[String] in scope
  val emptyString   = Monoid[String].empty
  val combineString = Monoid[String].combine("This is a ", "monoid")

  import cats.instances.option._ // construct an implicit Monoid[Option[Int]]
  val emptyOption: Option[Int] = Monoid[Option[Int]].empty
  val combineOption = Monoid[Option[Int]].combine(Option(2), Option.empty[Int]) // Some(2)
  val combineOption2 = Monoid[Option[Int]].combine(
    Option(2),
    Option(6)
  ) // Some(8), el compilador sabe como combinar el 2 y el 6 que están dentro de los Options

  // extension methods for Monoids - |+|
  // import cats.syntax.monad._ // la operación |+| se puede importar de aquí o de cats.syntax.semigroup._ ya que son la misma
  val combinedOptionFancy = Option(3) |+| Option(7)

  // todo: implement a reduceByFold like:
  // def combineFold[T](ls: List[T])(implicit monoid: Monoid[T]): T = ???
  def combineFold[T](ls: List[T])(implicit monoid: Monoid[T]): T =
    ls.foldLeft(monoid.empty)(_ |+| _)

  // todo: combine a list of phonebooks as Map[String, Int]
  // hint: don't construct a monoid - use an import
  val phonebooks = List(
    Map(
      "Alice" -> 234,
      "Bob"   -> 345
    ),
    Map(
      "Charlie" -> 897,
      "Daniel"  -> 9988
    ),
    Map(
      "Tina" -> 555
    )
  )

  import cats.instances.map._
  val combinedPhonebook  = phonebooks.foldLeft(Monoid[Map[String, Int]].empty)(_ |+| _)
  val combinedPhonebook2 = combineFold(phonebooks)

  // todo: shopping cart and online stores with Monoids
  case class ShoppingCart(items: List[String], total: Double)
  def checkout(shoppingCarts: List[ShoppingCart]): ShoppingCart = {
    implicit val shoppingCartMonoid: Monoid[ShoppingCart] =
      Monoid.instance[ShoppingCart](
        ShoppingCart(List.empty[String], 0.0),
        (s1, s2) => ShoppingCart(s1.items ++ s2.items, s1.total + s2.total)
      )
    combineFold(shoppingCarts)
  }

  val sCart1 = ShoppingCart(List("laptop", "mouse"), 1200.99)
  val sCart2 = ShoppingCart(List("t-shirt"), 45.65)

  def main(args: Array[String]): Unit = {
    println(sumLeft)
    println(sumRight)
    println(combineFold(numbers))
    println(combineFold(List("I ", "like ", "monoids")))
    println(combinedPhonebook)
    println(combinedPhonebook2)
    println(checkout(sCart1 :: sCart2 :: Nil))
  }
}
