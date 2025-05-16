package rock.cats

import scala.util.Try

/**
 * Monads
 */
object Lesson05_UsingMonads {

  import cats.Monad
  import cats.instances.list._
  import cats.instances.option._

  val monadList      = Monad[List]
  val aSimpleList    = monadList.pure(2)                                   // List(2)
  val anExtendedList = monadList.flatMap(aSimpleList)(x => List(x, x + 1)) // List(2, 3)
  // applicable to Option, Try, Future, etc.

  // either is also a monad
  val aManualEither: Either[String, Int] = Right(42)
  type LoadingOr[T] = Either[String, T]
  type ErrorOr[T]   = Either[Throwable, T]
  import cats.instances.either._
  val loadingMonad = Monad[LoadingOr]
  val anEither     = loadingMonad.pure(42) // LoadingOr[Int] == Right(42)
  val aChangedLoading = loadingMonad.flatMap(anEither)(n =>
    if (n % 2 == 0) Right(n + 1) else Left("Loading meaning of lifre...")
  )

  // imaginary online store
  case class OrderStatus(orderId: Long, status: String)
  def getOrderStatus(orderId: Long): LoadingOr[OrderStatus] = Right(
    OrderStatus(orderId, "Ready to ship")
  )
  def trackLocation(orderStatus: OrderStatus): LoadingOr[String] =
    if (orderStatus.orderId > 1000) Left("Not available yet, refreshing data ...")
    else Right("Location: 123 Main St, Springfield")

  val orderId = 42L
  val orderLocation =
    loadingMonad.flatMap(getOrderStatus(orderId))(orderStatus => trackLocation(orderStatus))

  // use extension methods
  import cats.syntax.flatMap._
  import cats.syntax.functor._
  val orderLocationBetter =
    getOrderStatus(orderId).flatMap(orderStatus => trackLocation(orderStatus))
  val orderLocationFor = for {
    orderStatus <- getOrderStatus(orderId)
    location    <- trackLocation(orderStatus)
  } yield location

  // todo the service layer API of a web app
  case class Connection(host: String, port: String)
  val config = Map(
    "host" -> "localhost",
    "port" -> "8080"
  )

  trait HttpService[M[_]] {
    def getConnection(cfg: Map[String, String]): M[Connection]
    def issueRequest(connection: Connection, payload: String): M[String]
  }

  /*
    Con este método podemos obtener la response con cualquier M si tenemos la instancia
    implícita de Monad[M] en el scope. Por lo tanto en al main podemos usar este método
    para obtener la respuesta de los diferentes servicios.
   */
  def getResponse[M[_]](service: HttpService[M], payload: String)(implicit
      monad: Monad[M]
  ): M[String] = {
    for {
      connection <- service.getConnection(config)
      response   <- service.issueRequest(connection, payload)
    } yield response
  }
  // DO NOT CHANGE THE CODE

  /*
    Requirements:
    - if the host and port are found in the config map, then we'll return a M containing a connection with those values
      otherwise the method will fail, according to the logic of the type M
    - the issueRequest method returns a M containing the string: "request (payload) has been accepted", if the payload is less than 20 characters,
      otherwise the method will fail, according to the logic of the type M

      TODO: provide a real implementation of HttpService using Try, Option, Future, Either, etc.
   */
  object TryHttpService extends HttpService[Try] {
    override def getConnection(cfg: Map[String, String]): Try[Connection] =
      Try {
        val host = cfg("host")
        val port = cfg("port")
        Connection(host, port)
      }

    override def issueRequest(connection: Connection, payload: String): Try[String] = {
      payload.length match {
        case l if l < 20 => Try(s"request ($payload) has been accepted")
        case _           => Try(throw new Exception("Payload too long"))
      }
    }
  }

  object OptionHttpService extends HttpService[Option] {
    override def getConnection(cfg: Map[String, String]): Option[Connection] =
      for {
        host <- cfg.get("host")
        port <- cfg.get("port")
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): Option[String] = {
      payload.length match {
        case l if l < 20 => Some(s"request ($payload) has been accepted")
        case _           => None
      }
    }
  }

  val responseOption = OptionHttpService.getConnection(config).flatMap { connection =>
    OptionHttpService.issueRequest(connection, "Hello, HTTP service")
  }

  val responseOptionFor = for {
    conn     <- OptionHttpService.getConnection(config)
    response <- OptionHttpService.issueRequest(conn, "Hello, HTTP service")
  } yield response

  // todo: implement another service using LoadingOr or ErrorOr
  // type ErrorOr[T]   = Either[Throwable, T]
  object ErrorOrHttpService extends HttpService[ErrorOr] {
    override def getConnection(cfg: Map[String, String]): ErrorOr[Connection] =
      for {
        host <- cfg.get("host").toRight(new RuntimeException("Host not found"))
        port <- cfg.get("port").toRight(new RuntimeException("Port not found"))
      } yield Connection(host, port)

    override def issueRequest(connection: Connection, payload: String): ErrorOr[String] = {
      payload.length match {
        case l if l < 20 => Right(s"request ($payload) has been accepted")
        case _           => Left(new RuntimeException("Payload too long"))
      }
    }
  }
  val errorOrResponse = ErrorOrHttpService.getConnection(config).flatMap { connection =>
    ErrorOrHttpService.issueRequest(connection, "ErrorOR service")
  }

  def main(args: Array[String]): Unit = {
    // try
    println("--Try--")
    println(TryHttpService.getConnection(config))
    println(TryHttpService.getConnection(Map("host" -> "localhost")))
    println(TryHttpService.issueRequest(Connection("localhost", "8080"), "Hello world"))
    println(
      TryHttpService.issueRequest(
        Connection("localhost", "8080"),
        "Hello world bla bla bla bla bla bla bla"
      )
    )

    // option
    println("--Option--")
    println(OptionHttpService.getConnection(config))
    println(OptionHttpService.getConnection(Map("host" -> "localhost")))
    println(OptionHttpService.issueRequest(Connection("localhost", "8080"), "Hello world"))
    println(
      OptionHttpService.issueRequest(
        Connection("localhost", "8080"),
        "Hello world bla bla bla bla bla bla bla"
      )
    )
    println(responseOption) // Some(request (Hello, HTTP service) has been accepted)

    // errorOr
    println("--ErrorOr--")
    println(ErrorOrHttpService.getConnection(config))
    println(ErrorOrHttpService.getConnection(Map("host" -> "localhost")))
    println(ErrorOrHttpService.issueRequest(Connection("localhost", "8080"), "Hello world"))
    println(
      ErrorOrHttpService.issueRequest(
        Connection("localhost", "8080"),
        "Hello world bla bla bla bla bla bla bla"
      )
    )
    println(errorOrResponse) // Right(request (ErrorOR HTTP service) has been accepted)

    // Using the getResponse method
    println("--Using getResponse--")
    println(getResponse(TryHttpService, "Hello world"))
    println(getResponse(OptionHttpService, "Hello world"))
    println(getResponse(ErrorOrHttpService, "Hello world"))
  }
}
