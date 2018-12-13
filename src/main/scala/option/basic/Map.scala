package option.basic

import option.OptionsToUse.{opSimpleInt,opEmpty}

/**
  * El método map nos permite aplicar una función al contenido de un Option.
  * Resultados posibles
  * Some(x).map(f) = Some(f(x))
  * None.map(f) = None
  */
object Map extends App {

  println("Option.map")
  println(s"Se aplica la función sumar 8 al contenido del Option, devolviendo otro Option son la transformación: " +
    s"Some(Map(a -> b)).map(_.+(8)) = ${opSimpleInt.map(_ + 8)}")
  println(s"Otra forma de hacer lo mismo que antes es: " +
    s"Some(Map(a -> b)).map(num => num + 8) = ${opSimpleInt.map(num => num + 8)}")

  println(s"Si le aplicamos la función map a un None obtenemos None: " +
    s"None.map(_.toList) = ${opEmpty.map(_.toList)}")
}
