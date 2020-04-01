package adt

/**
  * El código del presente fichero expone un ejemplo para el uso de ADT( Abstract Data Type)
  * En concreto se quiere realizar un filtrado sobre una lista de enteros según unas condiciones
  * que pueden combinarse con diferentes operaciones booleanas: and, or etc.
  */
object Calculus extends App {
  val input = List(1,2,3,4,5,6,7).map(_.asInstanceOf[Long])
  val conditions = List(Condition("greaterThan2"), Condition("par"))
  val operation = Operation("and")

  val response = Calculus(input, conditions, operation).apply

  println(response)
}

/**
  * La clase Calculus es la encargada de orquestar la construcción de la condición para filtrar la
  * lista además de ejecutar dicho filtrado
  * @param ls Lista de número a procesar
  * @param conds Lista de condiciones a aplicar en el fitlrado
  * @param op Operación Booleana para concatenar los diferentes filtros
  */
case class Calculus(ls: List[Long], conds: List[Condition], op: Operation) {
  def apply: List[Long] = {
    val conditionFunctions = conds.map(_.apply)
    val finalCondition =  op.apply(conditionFunctions.head, conditionFunctions.tail: _*)
    ls.filter(finalCondition(_))
  }
}

/**
  * Definición de una Condición con su método apply que devuelve, en función de la condición que sea,
  * un función (Long => Boolean) que se aplicará cuando se informe el argumento de entrada
  * @param id Nombre de la condición
  */
case class Condition(id: String) {
  def apply: Long => Boolean = {
    id match {
      case "greaterThan2" => x: Long => x > 2
      case "par" => x: Long => x % 2 == 0
    }
  }
}

/**
  * Operación para concatenar las funciones, consta del nombre de la operación y un método apply que
  * dado una lista de funciones (condiciones) las une en una usando el operador lógico indicado
  * @param op Nombre de la operación
  */
case class Operation(op: String) {
  def apply[A](f: A => Boolean, fs: A => Boolean *): A => Boolean =
    op match {
      case "and" => And(f, fs: _*).apply(_)
      case "or" => Or(f, fs: _*).apply(_)
      case "id" => Id(f).apply(_)
    }
}

/**
  * Operador And
  * @param p predicado
  * @param ps resto de predicados
  * @tparam A Tipo genérico
  */
case class And[A](p: A => Boolean, ps: A => Boolean *) {
  def apply(a: A): Boolean = p(a) && ps.map(_(a)).reduce(_ && _)
}
/**
  * Operador Or
  * @param p predicado
  * @param ps resto de predicados
  * @tparam A Tipo genérico
  */
case class Or[A](p: A => Boolean, ps: A => Boolean *) {
  def apply(a: A): Boolean = p(a) || ps.map(_(a)).reduce(_ || _)
}
/**
  * Operador Identity
  * @param p predicado
  * @param ps resto de predicados
  * @tparam A Tipo genérico
  */
case class Id[A](p: A => Boolean) {
  def apply(a: A): Boolean = p(a)
}
