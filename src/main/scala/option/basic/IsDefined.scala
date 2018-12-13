package option.basic

import option.OptionsToUse.{opSimpleMap,opEmpty}

/**
  * El método isDefined nos permite saber si un Option está bien definido(Some) o no(None)
  * Resultados posibles
  * Some(x).isDefined = true
  * None.isDefined = false
  */
object IsDefined extends App {

  println("Option.isDefined")
  println(s"Se comprueba que el option está bien definido (Some(algo) con la función isDefined: " +
    s"Some(Map(a -> b)).isDefined = ${opSimpleMap.isDefined}")
  println(s"Se comprueba que el option no está bien definido(None) con la función isDefined: " +
    s"None.isDefined = ${opEmpty.isDefined}")

}
