package tagless.shopping

import tagless.shopping.algebras.ShoppingCarts.{ScRepoState, ShoppingCartWithDependencyInterpreter}
import tagless.shopping.domain.Product
import tagless.shopping.programs.{Program, ProgramWithDep}
import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {

    val program: ProgramWithDep[ScRepoState] = ProgramWithDep {
      ShoppingCartWithDependencyInterpreter.make()
    }

    val cart = program.createAndToCart(Product("id", "a product"), "cart1")

    for {
      a <- program.createAndToCart(Product("id", "a product"), "cart1")
    } yield a


    IO.pure(ExitCode.Success)


//    for {
//      cart =
//      _ <- IO.println(cart.get)
//    } yield ExitCode.Success

  }

}
