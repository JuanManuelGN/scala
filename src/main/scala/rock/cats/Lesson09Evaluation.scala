package rock.cats

object Lesson09Evaluation {

  /*
    Cats makes the distinction between
    - evaluation an expression eagerly                    -> instantEval
    - evaluating lazily and every time you request it     -> redoEval
    - evaluating lazily and keeping the value (memoizing) -> delayedEval
   */

  import cats.Eval
  val instantEval = Eval.now {
    println("computing now!")
    65
  }

  val redoEval = Eval.always {
    println("computing again!")
    45
  }

  val delayedEval = Eval.later {
    println("computing later!")
    4
  }

  val composedEvaluation = instantEval.flatMap(v1 => delayedEval.map(v2 => v1 + v2))

  val composedEvaluationFor = for {
    v1 <- instantEval
    v2 <- delayedEval
  } yield v1 + v2

  // TODO: What gets printed if I execute it twice ?
  val evalEx1 = for {
    a <- delayedEval //
    b <- redoEval
    c <- instantEval
    d <- redoEval
  } yield a + b + c + d

  // remember a computed value
  val dontRecompute = redoEval.memoize

  val tutorial =
    Eval
      .always { println("step 1... "); "put the guitar on your lap" }
      .map { step1 => println("step 2"); s"$step1 then put your left hando on the neck" }
      .memoize // remember the value up to this point
      .map { steps21 =>
        println("step 3, more complicated"); s"$steps21 then with the right hand strike the strings"
      }

  // TODO: implement defer such that defer(Eval.now) does not run the side effects
  def defer[T](eval: => Eval[T]): Eval[T] = {
    // my solution
//    Eval.later(eval.value)
    // course solution
    Eval.later(()).flatMap(_ => eval)
  }

  // TODO: rewrite reverse method with Evals
  def reverseList[T](ls: List[T]): List[T] =
    if (ls.isEmpty) ls
    else reverseList(ls.tail) :+ ls.head

  def reverseEval[T](ls: List[T]): Eval[List[T]] = {
    // my solution, si usamos una lista de 10000 numeros falla con java.lang.StackOverflowError
//    if (ls.isEmpty) Eval.later(ls)
//    else reverseEval(ls.tail).flatMap(rs => Eval.later(rs :+ ls.head))
    // course solution, si usamos una lista de 10000 numeros falla con java.lang.StackOverflowError
//    if (ls.isEmpty) Eval.now(ls)
//    else reverseEval(ls.tail).map(_ :+ ls.head)
    // course safe solution using defer. Under the hood use tail recursion
    if (ls.isEmpty) Eval.now(ls)
    else defer(reverseEval(ls.tail).map(_ :+ ls.head))
  }

  def main(args: Array[String]): Unit = {
//    // Para obtener el valor de instantEval:
//    println(instantEval.value)
//
//    // Si ejecutamos dos veces redoEval obtenemos la evaluación de la expresión dos veces
//    println(redoEval.value)
//    println(redoEval.value)
//
//    // Si ejecutamos sin llamar a delayed.value la expresión no se evalua. Si se evalua cuando hacemos:
//    println(delayedEval.value)
//    // Si lo llamamos dos veces solo se imprime una vez computing later!, es decir, funciona como una cache
//    println(delayedEval.value)

    // Si evaluamos esta composicion formada por instant and delayed tenemos que solo se imprime una vez cada texto
//    println(composedEvaluation.value)
//    println(composedEvaluation.value)
//
//    println(composedEvaluationFor.value)

    // TODO: recuerda comentar todo lo que haya en el main antes
//    println(evalEx1.value)
    /* ^^^
      val evalEx1 = for {
        a <- delayedEval
        b <- redoEval
        c <- instantEval
        d <- redoEval
      } yield a + b + c + d
      Imprime:
      computing now! es la primera porque se evalua en el momento de la creción de Eval
      computing later!
      computing again!
      computing again!
     */

//    println(evalEx1.value)
    /*
      Imprime:
      computing again!
      computing again!
     */

    // Solo se evalua una vez aunque  redoEval diga que se haga siempre, esto es por el memoize
//    println(dontRecompute.value)
//    println(dontRecompute.value)

//    println(tutorial.value)
//    println(tutorial.value)

    defer(Eval.now {
      println("now!")
      42
    })

//    println(reverseEval(List(1, 2, 3, 4)).value)

    println(reverseEval((1 to 10000).toList).value)
  }
}
