package futures

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, NANOSECONDS}
import scala.util.{Failure, Success, Try}

/**
  * Comprobación de que los futuros se lanzan de manera secuencial por defecto, debiendo
  * de esperar la resolución del anterior para lanzar el siguiente.
  * Esto ocurre independientemente del tipo de declaración del futuro; val o lazy val
  */
object Futures {
  val future1: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      1
    }
  val future2: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      2
    }
  val future3: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      3
    }

  lazy val lazyFuture1: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      1
    }
  lazy val lazyFuture2: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      2
    }
  lazy val lazyFuture3: Future[Int] =
    Future.successful {
      Thread.sleep(1000)
      3
    }
}

object FuturesMain extends App {
  val time0 = System.nanoTime()
  val sumFutures =
    for {
      a <- Futures.future1
      b <- Futures.future2
      c <- Futures.future3
    } yield a + b + c


  val r =
    Try(Await.result(sumFutures, 5.seconds)) match {
      case Success(value) => value
      case Failure(exception) => -1
    }

  val time1 = System.nanoTime()

  println(s"Result $r Time expends in processing futures ${NANOSECONDS.toSeconds(time1 - time0)} seconds")

  // --------------------------------
  val lazyTime0 = System.nanoTime()
  val lazySumFutures =
    for {
      a <- Futures.lazyFuture1
      b <- Futures.lazyFuture2
      c <- Futures.lazyFuture3
    } yield a + b + c


  val lazyR =
    Try(Await.result(lazySumFutures, 5.seconds)) match {
      case Success(value) => value
      case Failure(exception) => -1
    }

  val lazyTime1 = System.nanoTime()

  println(s"Resutl $lazyR Time expends in processing lazy futures ${NANOSECONDS.toSeconds(lazyTime1 - lazyTime0)} seconds")
}
