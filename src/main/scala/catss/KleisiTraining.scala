package catss

import cats.data.Kleisli

/**
  * Entrenamiento con cats.Kleisi
  */
object KleisiTraining {

  /**
    * Tenemos dos funciones una que transforma un cadena a entero
    * y otra que divide el 1.0 entre un entero pasado por parámetro
    * las dos devuelven un Option[_]
    * ¿Qué pasa si las intentamos componer?
    */
  /**
    * Transforma una cadena a entero
    */
  val parse: String => Option[Int] =
    s => if (s.matches("-?[0-9]+")) Some(s.toInt) else None

  /**
    * Divide el número 1.0 entre el entero pasado como parámetro
    */
  val reciprocal: Int => Option[Double] =
    i => if (i != 0) Some(1.0 / i) else None

  /**
    * La intentamos componer,
    * como se puede observar si se descomenta la siguiente línea obtenemos un error de
    * compilación siendo imposible componer estas funciones sin cambiar su signatura
    * puesto que los tipos son incompatibles
    */
//  val reciParse = reciprocal.compose(parse)

  /**
    * Para resolver esto podemos usar la clase Kleisi de cats
    */

  import cats.implicits._

  val parseK: Kleisli[Option,String,Int] =
    Kleisli((s: String) => if (s.matches("-?[0-9]+")) Some(s.toInt) else None)

  val reciprocalK: Kleisli[Option,Int,Double] =
    Kleisli((i: Int) => if (i != 0) Some(1.0 / i) else None)

  val parseAndReciprocalK: Kleisli[Option,String,Double] =
    reciprocalK compose parseK
}

object KleisiTrainingRunner extends App {
//  println(
//    KleisiTraining.reciParse("4")
//  )

  println(
    KleisiTraining.parse("4")
  )

  println(
    KleisiTraining.reciprocal(4)
  )

  println(
    KleisiTraining.parseAndReciprocalK("4")
  )
}
