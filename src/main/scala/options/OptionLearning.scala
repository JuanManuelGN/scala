package options

case class OptionLearning(options: Option[Map[String, String]])

object OptionLearningRunner extends App{
  val op: OptionLearning = OptionLearning(Some(Map("secondPartition" -> "columnOne")))
  val opEmpty: OptionLearning = OptionLearning(None)
  val opSomeEmpty: OptionLearning = OptionLearning(Some(Map("secondPartition" -> "")))

  if (!op.options.fold("")(map => map("secondPartition")).isEmpty) {
    println(s"SecondPartition = ${op.options.get("secondPartition")}")
  }
  if (!opEmpty.options.fold("")(map => map("secondPartition")).isEmpty) {
    println(s"SecondPartition = ${opEmpty.options.get("secondPartition")}")
  } else {
    println("Empty")
  }
  if (!opSomeEmpty.options.fold("")(map => map("secondPartition")).isEmpty) {
    println(s"SecondPartition = ${opSomeEmpty.options.get("secondPartition")}")
  } else {
    println("Some(\"\")")
  }
}
