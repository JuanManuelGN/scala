package option

object OptionsToUse {

  val opEmpty: Option[Map[String,String]] = None
  val opSimpleMap: Option[Map[String,String]] = Some(Map("a" -> "b"))
  val opSimpleInt: Option[Int] = Some(4)
}
