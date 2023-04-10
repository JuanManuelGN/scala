package http

import cats.syntax.all._
import io.circe._
import io.circe.syntax._
import io.circe.generic.semiauto._
import cats.effect._
import org.http4s._
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.client.Client
import cats.effect.unsafe.IORuntime
implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global

object FifteenFiveClientInterpreterSpec extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    case class User(name: String, age: Int)
    implicit val UserEncoder: Encoder[User] = deriveEncoder[User]

    val user = User("name", 18)

    def httpRoutes[F[_]]()(implicit
                           F: Async[F]
    ): HttpRoutes[F] = HttpRoutes.of[F] { case GET -> Root / "user" / id =>
      F.pure(Response(status = Status.Ok).withEntity(user.asJson))

    }

    val httpApp: HttpApp[IO] = httpRoutes[IO]().orNotFound

    val request: Request[IO] = Request(method = Method.GET, uri = uri"/user/not-used")
    val client: Client[IO] = Client.fromHttpApp(httpApp)

    val resp: IO[Json] = client.expect[Json](request)

    println(
      resp.unsafeRunSync()
    )

    for
      _ <- resp
    yield ExitCode.Success
  }
}

import cats.syntax.all.*
import io.circe.*
import io.circe.syntax.*
import io.circe.generic.semiauto.*
import cats.effect.*
import org.http4s.*
import org.http4s.circe.*
import org.http4s.dsl.io.*
import org.http4s.implicits.*
import org.http4s.client.Client
import cats.effect.unsafe.IORuntime
import com.xebia.functional.integrations.fifteenfive.config.FifteenFiveConfig
import com.xebia.functional.integrations.fifteenfive.models.ManagerId
implicit val runtime: IORuntime = cats.effect.unsafe.IORuntime.global
import com.xebia.functional.integrations.fifteenfive.TestData.readJson
import io.circe.literal.json

object FifteenFiveClientInterpreterRunSpec extends IOApp {

  def run(args: List[String]): IO[ExitCode] = {

    object UserIDQueryParamMatcher extends QueryParamDecoderMatcher[String]("user_id")
    object StateQueryParamMatcher extends QueryParamDecoderMatcher[String]("state")

    val objectives = readJson("fifteen_objectives.json")
    def httpRoutes[F[_]]()(implicit
                           F: Async[F]
    ): HttpRoutes[F] = HttpRoutes.of[F] {
      case GET -> Root / "objective" :?
        UserIDQueryParamMatcher(_) +&
          StateQueryParamMatcher(_) =>
        F.pure(Response(status = Status.Ok).withEntity(json"""$objectives"""))
    }

    val httpApp: HttpApp[IO] = httpRoutes[IO]().orNotFound
    val client: Client[IO] = Client.fromHttpApp(httpApp)

    val config: FifteenFiveConfig =
      new FifteenFiveConfig(uri"", "token", 10, 10)

    val fifteenFiveAlg = new FifteenFiveClientInterpreter(config, client)

    val resp = fifteenFiveAlg.getObjective(ManagerId("3"))

    IO.println(
      resp.unsafeRunSync())
    for
      a <- IO.pure(4)
    yield {
      ExitCode.Success
    }
  }
}