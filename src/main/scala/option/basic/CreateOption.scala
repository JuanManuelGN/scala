package option.basic

/**
  * En esta clase se van a explorar las diferentes formas de crear Option[A]
  */
object CreateOption extends App {

  val nilString = Nil
  val emptyString = ""
  val s = "im String"

  val nilOption = Option(nilString)
  println(s"Se ha creado un Option con Option(Nil) = ${nilOption}")
  /**
    * Como se puede observar se crea como un Some(List()) y nuestra intención inicial era que se
    * creara como un None sin realizar comprobaciones tipo if nilString != ""....
    */

  /**
    * Ahora vamos a crear un Option con usando un filter para obtener None cuando el String sea
    * la cadena vacía o Nil y Some cuando la cadena esté bien definida
    */
  val emptyOption = Option(emptyString).filter(_.nonEmpty)
  println("Se ha creado un Option con Option(\"\").filter(_.nonEmpty) = " +
    s"${emptyOption}")

  val nilOptionFilter = Option(nilString).filter(_.nonEmpty)
  println(s"Se ha creado un Option con Option(Nil).filter(_.nonEmpty) = ${nilOptionFilter}")

  val sOption = Option(s).filter(_.nonEmpty)
  println("Se ha creado un Option con Option(\"im String\").filter(_.nonEmpty) = " +
    s"${sOption}")
}
