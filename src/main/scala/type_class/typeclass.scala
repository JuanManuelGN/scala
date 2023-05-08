package type_class

import type_class.typeclass.{Addable, Group}

object typeclass {

  case class Group(name: String, people: Set[String])

  trait Addable[T] {
    def +(a: T, b: T): T
  }

  object Addables {
    implicit object AddableInt extends Addable[Int] {
      def +(a: Int, b: Int): Int = a + b
    }

    implicit object AddableString extends Addable[String] {
      def +(a: String, b: String): String = a + b
    }

    implicit object AddableBoolean extends Addable[Boolean] {
      def +(a: Boolean, b: Boolean): Boolean = a && b
    }

    /**
      * Otra forma de escribir el implicit, en vez de extendiendo creando una instancia
      * del trait Addable
      */
    implicit val addableDouble: Addable[Double] = new Addable[Double] {
      def +(a: Double, b: Double): Double = a + b
    }

    implicit object AddableGroup extends Addable[Group] {
      def +(a: Group, b: Group): Group =
        Group(
          name = a.name + " & " + b.name,
          people = a.people | b.people
        )
    }

    /**
      * Otra forma de escribirlo, así se consigue que a la hora de llamar para hacer la
      * suma se pueda usar una sintaxis del tipo 4 + 5, ver mas abajo en el main.
      */
    implicit class AddableSyntax[T](x: T) {
      def ++(y: T)(implicit ev: Addable[T]): T = ev.+(x, y)
    }
  }
}

object run extends App {
  import type_class.typeclass.Addables._

  def plus[T](a1: T, a2: T)(implicit ev: Addable[T]): T = ev.+(a1, a2)

  /**
    * Usando AddableSyntax
    */
  val a1 = 1
  val a2 = 1
  val r = a1 ++ a2
  println(r)

  println(
    plus(true, false)
  )
  println(
    plus("true", "false")
  )
  println(
    plus(Group("A", Set("Peter")), Group("B", Set("John, Chris")))
  )
  println(
    plus(2.3, 1.7)
  )

}
