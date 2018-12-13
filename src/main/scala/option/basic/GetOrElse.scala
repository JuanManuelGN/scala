package option.basic

import option.OptionsToUse.{opSimpleMap,opEmpty}

/**
  * El método getOrElse nos permite obtener el valor que contiene el Option o un valor que le indiquemos
  * por dafecto en el caso de que el option valga None.
  * Resultados posibles
  * Some(x).getOrElse("default") = x
  * None.getOrElse("default") = "default"
  */
object GetOrElse extends App {

  println("Option.getOrElse")
  println(s"Se obtiene el valor del option con la función getOrElse: " +
    s"Some(Map(a -> b)).getOrElse('Empty') = ${opSimpleMap.getOrElse("Empty")}" +
    s". Puesto que el option contiene un valor correcto lo devuelve.")
  println(s"Se obtiene el valor del option con la función getOrElse: " +
    s"None.getOrElse(Empty) = ${opEmpty.getOrElse("Empty")}" +
    s". Puesto que el option no contiene un valor correcto devuelve Empty")

}
