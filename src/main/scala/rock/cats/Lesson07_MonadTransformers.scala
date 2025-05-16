package rock.cats

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import cats.instances.future._

/**
 * Monad Transformers
 * HKT for convenience over nested monadic values
 * OptionT and EitherT
 * use map and flatMap
 */
object Lesson07_MonadTransformers {

  def sumAllOptions(ops: List[Option[Int]]): Int = ???

  // option transformer
  import cats.data.OptionT
  import cats.instances.list._ // fetch an implicit OptionT[List]

  val listOfNumberOptions: OptionT[List, Int] = OptionT(List(Option(1), Option(2)))
  val listOfCharOptions: OptionT[List, Char]  = OptionT(List(Option('a'), Option('b'), Option('c')))
  val listOfTuples: OptionT[List, (Int, Char)] = for {
    char   <- listOfCharOptions
    number <- listOfNumberOptions
  } yield (number, char)

  // either transformer
  import cats.data.EitherT
  val listOfEithers: EitherT[List, String, Int] = EitherT(
    List(Left("something wrong"), Right(43), Right(2))
  )
  implicit val ec: ExecutionContext =
    ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val futureOfEither: EitherT[Future, String, Int] = EitherT[Future, String, Int](Future(Right(45)))
  // Con la siguiente forma de construirlo no hace flata poner el tipo
  val futureOfEither2: EitherT[Future, String, Int] = EitherT.right(Future(45))

  /*
    TODO
    We have a multi-machine cluster for your business which will receive a traffic surge following a media appearance.
    We measure bandwidth in units.
    We want to allocate TWO of servers to cope with the traffic spike.
    We know the current capacity for each server and we know we'll hold the traffic if the sum of bandwidths is  > 250.
   */
  val bandwidths = Map(
    "server1.navarrosoft.es" -> 50,
    "server2.navarrosoft.es" -> 300,
    "server3.navarrosoft.es" -> 170
  )

  type AsyncResponse[T] = EitherT[Future, String, T] // wrapper over Future[Either[String, T]]

  def getBandwidth(server: String): AsyncResponse[Int] = bandwidths.get(server) match {
    case None    => EitherT[Future, String, Int](Future(Left(s"$server unreachable")))
    case Some(b) => EitherT[Future, String, Int](Future(Right(b)))
  }

  // TODO
  // hint: call getBandwidth twice and combine the results
  
  def canWithstandSurge(s1: String, s2: String): AsyncResponse[Boolean] = for {
    band1 <- getBandwidth(s1)
    band2 <- getBandwidth(s2)
  } yield band1 + band2 > 250 // Future[Either[String, Boolean]]

  // hint: call canWithstandSurge + transform
  def generateTrafficSpikeReport(
      s1: String,
      s2: String
  ): AsyncResponse[String] = { // no pueden estar al mismo tiempo
    canWithstandSurge(s1, s2).transform {
      case Left(reason) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: $reason")
      case Right(false) => Left(s"Servers $s1 and $s2 CANNOT cope with the incoming spike: not enough total bandwidth")
      case Right(true) => Right(s"Servers $s1 and $s2 can cope with the incoming spike")
    }
    //  ^^^^^^^^^^^^^^              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //  Future[Either[String, Boolean]] -- transform --> Future[Either[String, String]]
  }

  def main(args: Array[String]): Unit = {
    println(listOfTuples.value)

    val resultFuture = generateTrafficSpikeReport("server1.navarrosoft.es", "server3.navarrosoft.es").value
    resultFuture.foreach(println)

    val resultFuture2 = generateTrafficSpikeReport("server5.navarrosoft.es", "server3.navarrosoft.es").value
    resultFuture2.foreach(println)

    val resultFuture3 = generateTrafficSpikeReport("server2.navarrosoft.es", "server3.navarrosoft.es").value
    resultFuture3.foreach(println)
  }

}
