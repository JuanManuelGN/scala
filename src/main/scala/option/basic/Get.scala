//package option.basic
//
//import option.OptionsToUse.{optSimpleMap,optEmpty}
//
//import scala.util.Try
//
///**
//  * El método get es el má básico de todos, nos permite obtener el valor que contiene el Option.
//  * Un option se forma mediante dos contructores Some y None
//  * Some(x) donde x es cualquier objeto
//  * None para indicar que el option está vacío
//  * Resultados posibles
//  * Some(x).get = x
//  * None.get = java.util.NoSuchElementException
//  */
//object Get extends App {
//
//  println("Option.get")
//  println(s"Se obtiene el valor del option con la función get: Some(Map(a -> b)).get = ${optSimpleMap.get}")
//  println(s"Se intenta obtiener el valor del option con contenido None(val opEmpty: Option[Map[String,String]] = None)" +
//    s" con la función get:None.get = ${Try(optEmpty.transpose.get)}" +
//    s". Se puede comprobar que ha dado una java.util.NoSuchElementException")
//}
