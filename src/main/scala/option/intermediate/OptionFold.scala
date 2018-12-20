package option.intermediate

import option.OptionsToUse.{optSimpleMap, optEmpty}

object OptionFold extends App {

    /**
      * Dado un Option con una mapa dentro, se quiete obtener el valor que contiene
      * el mapa para una key dada. Es posible que el Option venga a None, que venga
      * bien definido, o que venga sin la key que buscamos.
      * Obtener el valor buscado en una única línea de código que devuelva "notFound"
      * cuando no encontremos la key buscada por cualquier de los motivos antes
      * expuestos.
      *
      * Para ello vamos usar la función fold de la clase Option. Esta función tiene el
      * siguiente protocolo de ejecución. Option.fold(caso base)(caso general)
      * 1. Cuando el Option al que se le aplica el fold vale None, se ejecuta el bloque
      *    de código que se escribe en su caso base.
      * 2. Cuando el Option viene definido: Some(...) se ejecuta el bloque de código de
      *    su caso general
      * */
  println(s"Se obtiene el valor de una llave de un mapa encapsulado en un Option: " +
    "Some(Map(\"a\" -> \"b\"))" +
    ".fold(\"notFound\")(map => map.getOrElse(\"a\",\"notFound\")) = " +
    s"${optSimpleMap.fold("notFound")(map => map.getOrElse("a","notFound"))}")
  println("")

  println(s"Identica prueba pero esta vez la llave no se encuentra en el Map: " +
    "Some(Map(\"a\" -> \"b\"))" +
    ".fold(\"notFound\")(map => map.getOrElse(\"c\",\"notFound\")) = " +
    s"${optSimpleMap.fold("notFound")(map => map.getOrElse("c","notFound"))}")
  println("")

  println(s"Identica prueba pero esta vez el Map está vacio, es decir que su valor es None: " +
    s"None" +
    ".fold(\"notFound\")(map => map.getOrElse(\"c\",\"notFound\")) = " +
    s"${optEmpty.fold("notFound")(map => map.getOrElse("a","notFound"))}")
  println("")
}
