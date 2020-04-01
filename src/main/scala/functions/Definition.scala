package functions

/**
  * En esta clase se van a explorar las diferentes maneras de declarar una función
  */
object Definition extends App {

  val f: String => Int => Int = a => b => a.toInt + b
  val g: (String, Int) => Int = (a, b) => a.toInt + b
  val h = (a: String) => (b: Int) => a.toInt + b

  println(f("3"))
  println(f("3")(4))

  /** La función g no puede aplicarse parcialemnte */
  println(g("3", 4))

  println(h("3"))
  println(h("3")(4))

}
