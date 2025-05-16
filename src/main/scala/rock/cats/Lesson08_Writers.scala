package rock.cats
import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

object Lesson08_Writers {

  import cats.data.Writer
  // 1 - define them at the start
  val aWriter: Writer[List[String], Int] = Writer(List("Started something"), 45)
  // 2 - manipulate them with pure FP
  val aIncreasedWriter = aWriter.map(_ + 1) // value increases, logs stay the same
  val aLogsWriter =
    aWriter.mapWritten(_ :+ "found something interesting") // value stays the same, logs change
  val aWriterWithBoth = aWriter.bimap(_ :+ "logs change", _ + 1) // both value and logs change
  val aWriterWithBoth2 = aWriter.mapBoth { (logs, value) =>
    (logs :+ "logs chango by mapBoth", value + 1)
  }

  import cats.instances.vector._ // import Semigroup[Vector] para concatenar los logs
  val writerA = Writer(Vector("log a1", "log a2"), 10)
  val writerB = Writer(Vector("log b1"), 40)
  val compositeWriter = for {
    va <- writerA
    vb <- writerB
  } yield va + vb

  // reset the logs
  import cats.instances.list._ // a implicit Monoid[List[Int]]
  val anEmptyWriter = aWriter.reset // clear the logs, keep the value

  // 3 - dump either the value or logs
  val desireValue = aWriter.value
  val logs        = aWriter.written
  val (l, v)      = aWriter.run

  // TODO: rewrite a function which prints things with writers
  def countAndSay(n: Int): Unit = {
    if (n <= 0) println("starting!")
    else {
      countAndSay(n - 1)
      println(n)
    }
  }

  /** Tiene que hacer lo mismo que countAndSay pero almacenando los logs en Vector[String]
    */
  def countAndLog(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector("starting!"), 0)
    else countAndLog(n - 1).flatMap(_ => Writer(Vector(n.toString), n))
  }

  // Benefit: we work fith pure FP

  // TODO: countAndLog using tailrec

  // TODO: rewrite this methodd with writers
  def naiveSum(n: Int): Int = {
    if (n <= 0) 0
    else {
      println(s"Now at $n")
      val lowerSum = naiveSum(n - 1)
      println(s"Computed sum(${n - 1}) = $lowerSum")
      lowerSum + n
    }
  }

  /*
    Falla en el orden de impresión
    Now at 4
    Now at 3
    Now at 2
    Now at 1
    Computed sum(0) = 0
    Computed sum(1) = 1
    Computed sum(2) = 3
    Computed sum(3) = 6
    Now at 5
    Computed sum(4) = 10
   */
  def naiveSumWithWriter(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty[String], 0)
    else {
      val lowerSum = naiveSum(n - 1)
      Writer(Vector(s"Now at $n", s"Computed sum(${n - 1}) = $lowerSum"), lowerSum + n)
    }
  }
  // course solution
  // Benefit: Writers can keep logs separate on multiple threads
  def sumWithLogs(n: Int): Writer[Vector[String], Int] = {
    if (n <= 0) Writer(Vector.empty[String], 0)
    else {
      for {
        _        <- Writer(Vector(s"Now at $n"), n)
        lowerSum <- sumWithLogs(n - 1)
        _        <- Writer(Vector(s"Computed sum(${n - 1}) = $lowerSum"), n)
      } yield lowerSum + n
    }
  }

  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  def main(args: Array[String]): Unit = {
    println(compositeWriter.run)
    println(countAndLog(5))
    countAndLog(3).written.foreach(println)
    naiveSum(5)
    naiveSumWithWriter(5).written.foreach(println)
    sumWithLogs(5).written.foreach(println)

    /*
      Ejecutando las siguientes dos instrucciones no sabemos diferenciar
      los logs de cada uno de los threads:
      Computed sum(53) = 1431
      Computed sum(54) = 1485
      Computed sum(21) = 231
      Computed sum(55) = 1540
     */
    Future(naiveSum(100)).foreach(println)
    Future(naiveSum(100)).foreach(println)

    /*
      Con las siguientes lineas conseguimos saber la procedencia de cada log
      porque estamos usando la clase Writer
     */
    val sumFuture1 = Future(sumWithLogs(20))
    val sumFuture2 = Future(sumWithLogs(20))
    val logs1      = sumFuture1.map(_.written) // logs from thread 1
    val logs2      = sumFuture2.map(_.written) // logs from thread 2
    logs1.foreach(println)
    logs2.foreach(println)
  }

}
