package option.basic

import option.OptionsToUse.{optSimpleMap,optEmpty}

/**
  * El método isEmpty nos permite saber si un Option está bien definido(Some) o no(None)
  * Resultados posibles
  * Some(x).isEmpty = false
  * None.isEmpty = true
  */
object IsEmpty extends App {

  println("Option.isDefined")
  println(s"Se comprueba que el option está vacio(None) con la función isDefined: " +
    s"None.isEmpty = ${optEmpty.isEmpty}")
  println(s"Se comprueba que el option no está vacio(Some(algo)) con la función isEmpty: " +
    s"Some(Map(a -> b)).isDefined = ${optSimpleMap.isEmpty}")

}
