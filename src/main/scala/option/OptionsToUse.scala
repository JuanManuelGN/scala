package option

object OptionsToUse {

  val optEmpty: Option[Map[String,String]] = None
  val optSimpleMap: Option[Map[String,String]] = Some(Map("a" -> "b"))
  val optSimpleInt: Option[Int] = Some(4)
}
