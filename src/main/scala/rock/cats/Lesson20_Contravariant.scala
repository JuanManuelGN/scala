package rock.cats

object Lesson20_Contravariant {

  trait Format[A] {
    def format(v: A): String
  }

  def format[A](v: A)(implicit f: Format[A]) = f.format(v)

  implicit object StringFormat extends Format[String] {
    override def format(v: String) = "\"" + v + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(v: Int) = v.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(v: Boolean) = if (v) "Y" else "N"
  }

  // Problem: given Format[MyType], can we have a Format[Option[MyType]] automatically ?
//  implicit def getOptionFormat[T](implicit f: Format[T]): Format[Option[T]] =
//    new Format[Option[T]] {
//      override def format(v: Option[T]) = f.format(v.get) // .get --> this is bad
//    }

  // A = Option[T] and func = .get
//  def contramap[A, T](func: A => T)(implicit f: Format[T]): Format[A] = new Format[A] {
//    override def format(v: A) = f.format(func(v))
//  }

  // por lo tanto getOptionFormat es un contramap
//  implicit def getOptionFormatCM[T](implicit f: Format[T]): Format[Option[T]] =
//    contramap[Option[T], T](_.get)

  trait Format2[T] { self => // contravariant type classes
    def format(v: T): String
    def contramap[A](func: A => T): Format2[A] = new Format2[A] {
      override def format(v: A) =
        self.format(func(v)) // es como usar el this pero con traits no se puede
    }
  }

  def format2[A](v: A)(implicit f: Format2[A]) = f.format(v)
  
  implicit object IntFormat2 extends Format2[Int] {
    override def format(v: Int) = v.toString
  }
//  implicit def getOptionFormatF2[T](implicit f: Format2[T]): Format2[Option[T]] =
//    f.contramap[Option[T]](_.get)

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]
  val showInts                      = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  // Para eliminar el .get usamos Monoid
  import cats.Monoid
  import cats.instances.option._
  implicit def getOptionFormatM[T](implicit f: Format2[T], m: Monoid[T]): Format2[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))
  
  def main(args: Array[String]): Unit = {
    println(format("hello"))
    println(format(3))
    println(format(true))
    println(format2(Option(34)))
    println(format2(Option.empty[Int]))
    println(format2(Option(Option(34)))) // este no me funciona
  }

}
