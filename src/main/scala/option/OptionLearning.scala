//package options.basicI
//
//import scala.util.Try
//
//case class OptionLearning(options: Option[Map[String, String]])
//
//object OptionLearningRunner extends App {
//  val op: OptionMap = OptionMap(Some(Map("secondPartition" -> "columnOne")))
//  val opEmpty: OptionMap = OptionMap(None)
//  val opSomeEmpty: OptionMap = OptionMap(Some(Map("secondPartition" -> "")))
//  val opSomeMMMMM: OptionMap = OptionMap(Some(Map("secondPartition" -> "")))
//
//  if (!op.options.fold("")(map => map("secondPartition")).isEmpty) {
//    println(s"SecondPartition = ${op.options.get("secondPartition")}")
//  }
//  if (!opEmpty.options.fold("")(map => map("secondPartition")).isEmpty) {
//    println(s"SecondPartition = ${opEmpty.options.get("secondPartition")}")
//  } else {
//    println("Empty")
//  }
//  if (!opSomeEmpty.options.fold("")(map => map("secondPartition")).isEmpty) {
//    println(s"SecondPartition = ${opSomeEmpty.options.get("secondPartition")}")
//  } else {
//    println("Some(\"\")")
//  }
//  if (opSomeMMMMM.options.isDefined) {
//    if (!opSomeMMMMM.options.get("secondPartition").isEmpty) {
//      println(s"SecondPartition = ${opSomeMMMMM.options.get("secondPartition")}")
//    }
//    else {
//      println("se tienen opciones pero no la secondpartition")
//    }
//  } else {
//    println("Some(MMMM)")
//  }
//  if (Try(!opSomeMMMMM.options.fold("")(map => map("secondPartition")).isEmpty).isSuccess) {
//    println(s"SecondPartition = ${opSomeMMMMM.options.get("secondPartition")}")
//  } else {
//    println("Some(WTF)")
//  }
//}
//
//object OptionBasic extends App {
//  /** Dado un Option con una mapa dentro, se quiete obtener el valor que contiene
//    * el mapa para una key dada. Es posible que el Option venga a None, que venga
//    * bien definido, o que venga sin la key que buscamos.
//    * Obtener el valor buscado en una única línea de código que devuelva notFound
//    * cuando no encontremos la key buscada por cualquier de los motivos antes
//    * expuestos
//    */
//  val mapOption: Option[Map[String,String]] = Some(Map("secondPartition" -> "lele"))
//  val mapOptionEmpty: Option[Map[String,String]] = None
//  val mapOptionWithoutSecondP: Option[Map[String,String]] = Some(Map("lala" -> "lele"))
//
//  val secondP = mapOptionWithoutSecondP.fold("notFound")(map => map.getOrElse("secondPartition","notFound"))
//  println(secondP)
//}
//
//object EliminateOptions extends App {
//  val map: Map[String,String] = Map("juan"->"garcia","pepe"->"gomez")
//  val juan = map - "juan"
//  val pepe = map - "pepe"
//  val nada = map - "juan" -"pepe"
////  println(juan)
////  println(pepe)
////  println(nada)
//  val error = nada - "jeje"
////  println(error)
//  val opt = Some(error)
////  println(opt.get.isEmpty)
//
//  println(opt.fold(Map[String,String]())(m => m - "juan" -"pepe"))
//
//}
