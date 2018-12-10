package options.basicI

import scala.util.Try

/**
  * La siguiente clase define un Option con un mapa dentro
  *
  * @param options
  */
object OptionLearn extends App {

  /** Se declaran varias variables con todos los casos posibles */
  val op: Option[Map[String,String]] = Some(Map("a" -> "b"))
  val opEmpty: Option[Map[String,String]] = None
  val opSomeWithoutValue: Option[Map[String,String]] = Some(Map("a" -> ""))
  val opSomeMMMMM: Option[Map[String,String]] = Some(Map("a" -> "b", "x" -> "y"))

  /** get */
  println("Option.get")
  println(s"Se obtiene el valor del option con la función get: op.get = ${op.get}")
  println(s"Se intenta obtiener el valor del option con contenido None(val opEmpty: Option[Map[String,String]] = None)" +
    s" con la función get:opEmpty.get = ${Try(opEmpty.get)}" +
    s". Se puede comprobar que ha dado una excepción")
  println("")

  /** getOrElse */
  println("Option.getOrElse")
  println(s"Se obtiene el valor del option con la función getOrElse: op.getOrElse('Empty') = ${op.getOrElse("Empty")}" +
    s". Puesto que el option contiene un valor correcto lo devuelve")
  println(s"Se obtiene el valor del option con la función getOrElse: opEmpty.getOrElse('Empty') = ${opEmpty.getOrElse("Empty")}" +
    s". Puesto que el option no contiene un valor correcto devuelve Empty")
  println("")

  /** isDefined */
  println("Option.isDefined")
  println(s"Se comprueba que el option está bien definido (Some(algo) con la función isDefined: op.isDefined = ${op.isDefined}")
  println(s"Se comprueba que el option no está bien definido(None) con la función isDefined: opEmpty.isDefined = ${opEmpty.isDefined}")
  println("")

  /** isEmpty */
  println("Option.isDefined")
  println(s"Se comprueba que el option está vacio(None) con la función isDefined: opEmpty.isEmpty = ${opEmpty.isEmpty}")
  println(s"Se comprueba que el option no está vacio(Some(algo)) con la función isEmpty: op.isDefined = ${op.isEmpty}")
  println("")
}
