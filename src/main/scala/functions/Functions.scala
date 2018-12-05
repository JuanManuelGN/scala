package functions

case class Functions() {

  val s: String => Int => Int = a => b => a.toInt + b // def(a:Int,b:Int):Int = a+b
  val m: Int => Int => String = a => b => (a * b).toString

  val sComposeM = s.compose(m(3))
}
