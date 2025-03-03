package rock.cats

/** Combinar elementos de similares tipos
  */
object Lesson01Semigroups {

  import cats.Semigroup
  import cats.instances.int._

  // La forma por defecto de combinar dos números es sumarlos
  val naturalIntSemigroup = Semigroup[Int]
  val intCombination      = naturalIntSemigroup.combine(3, 7) // addition 10

  import cats.instances.string._
  // La forma por defecto de combinar cadenas es contatenarlas
  val naturalStringSemigroup = Semigroup[String]
  val stringCombination      = naturalStringSemigroup.combine("Estoy de ", "camping")

  // Se usa la combinación natural de enteros (suma) para reducir una lista de números
  def reduceInts(list: List[Int]): Int = list.reduce(naturalIntSemigroup.combine)
  // igual para reducir una lista
  def reduceStrings(list: List[String]): String = list.reduce(naturalStringSemigroup.combine)

  // General API
  def reduceThings[T](list: List[T])(implicit semigroup: Semigroup[T]): T =
    list.reduce(semigroup.combine)

  // Exercise: support a new type. Build a Semigroup[Expense]
  // hint: use Semigroup.instance for create a new one
  case class Expense(id: Long, amount: Double)
  implicit val expenseSemigroup: Semigroup[Expense] =
    Semigroup.instance[Expense]((e1, e2) => Expense(e1.id, e1.amount + e2.amount))

  // extension methods from Semigroup - |+|
  import cats.syntax.semigroup._
  val anIntSum         = 2 |+| 3 // requires an implicit Semigroup[Int] declared in the scope
  val aStringConcat    = "we like" |+| "Scala"
  val aCombinedExpense = Expense(3, 45.8) |+| Expense(3, 45.8)

  // Exercise: implement reduceThings2 with the |+| function
  def reduceThings2[T](list: List[T])(implicit semigroup: Semigroup[T]): T = list.reduce(_ |+| _)
  def reduceThings3[T: Semigroup](list: List[T]): T =
    list.reduce(_ |+| _) // equivalent to the above

  def main(arg: Array[String]): Unit = {
    println(intCombination)
    println(stringCombination)
    println(reduceInts(1 :: 2 :: 3 :: Nil)) // 6
    val strings = "I'm " :: "starting " :: "with " :: "cats" :: Nil
    println(reduceStrings(strings))
    val numbers = (1 to 10).toList
    println(reduceThings(numbers)) // compiler inject the implicit Semigroup[Int]
    println(reduceThings(strings)) // compiler inject the implicit Semigroup[String]
    import cats.instances.option._
    // compiler will PRODUCE an implicit Semigroup[Option[Int]]    - combine will produce another option with the summed elements
    // compiler will PRODUCE an implicit Semigroup[Option[String]] - combine will produce another option with the concatenated elements
    val numberOpts: List[Option[Int]] = numbers.map(x => Option(x))
    println(reduceThings(numberOpts)) // an Option[Int] containing the sum of all the numbers
    val stringOptions: List[Option[String]] = strings.map(Option(_))
    println(reduceThings(stringOptions))

    val expense1 = Expense(1, 10.8)
    val expense2 = Expense(3, 45.8)
    val expense3 = Expense(5, 11)
    val expenses = List(expense1, expense2, expense3)
    println(expenseSemigroup.combine(expense1, expense2))
    println(
      reduceThings(expenses)
    )

    println(anIntSum)
    println(aStringConcat)
    println(aCombinedExpense)

    println(
      reduceThings2(expenses)
    )
    println(
      reduceThings3(expenses)
    )
  }

}
