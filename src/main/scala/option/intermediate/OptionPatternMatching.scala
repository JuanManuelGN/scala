package option.intermediate

import option.OptionsToUse.{optEmpty, optSimpleInt}

/**
  * El método map nos permite aplicar una función al contenido de un Option.
  * Resultados posibles
  * Some(x).map(f) = Some(f(x))
  * None.map(f) = None
  */
object OptionPatternMatching extends App {

  println("Pattern matching with options")
  val num = optSimpleInt match {
    case Some(x) => x+3
    case None => 0
  }
  println(s"Se aplica un pattern matching a un Option[Int] sumando 3 en el caso que sea Some(x) o devolviendo " +
    s"0 en el caso que el option valga None: El resultado es = $num para Some(4)")
}
