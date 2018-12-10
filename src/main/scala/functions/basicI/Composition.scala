package functions.basicI

object Composition extends App {

  /** Dadas las siguiente funciones se va a estudiar su composición */
  val f: String => Int => Int = a => b => a.toInt + b // def(a:Int,b:Int):Int = a+b
  val g: Int => Int => String = a => b => (a * b).toString

  val fComposeG = f.compose(g(0)) // matemáticamente equivalente a f(g(0,_),_) donde _ son valores sin determinar
  println(s"compose: $fComposeG")
  println(s"compose with one param: ${fComposeG(2)}") // equivalente a f(g(0,2),_)
  println(s"compose with two params: ${fComposeG(2)(3)}") // equivalente a f(g(0,2),3)
  /** Los parámetros se van colocanto en los huecos sin asignar de izquierda a derecha, es decir
    * si tenemos f(g(5,_),_) y lo parametrizamos con el valor 2, ese 2 va a parar aquí f(g(5,2),_) */

}
