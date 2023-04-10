package tagless.shopping.programs

import cats.Monad
import tagless.shopping.algebras.ShoppingCarts
import tagless.shopping.domain.{Product, ShoppingCart}
import cats.implicits._

object Program {

  // Using implicit objects (explicit)
  def createAndAddToCart[F[_] : Monad](product: Product, cartId: String)
                                      (implicit shoppingCarts: ShoppingCarts[F]): F[Option[ShoppingCart]] =
    for {
      _ <- shoppingCarts.create(cartId)
      maybeSc <- shoppingCarts.find(cartId)
      maybeNewSc <- maybeSc.traverse(sc => shoppingCarts.add(sc, product))
    } yield maybeNewSc

  // Using the summoned values pattern
  def createAndToCart[F[_] : Monad : ShoppingCarts](product: Product, cartId: String): F[Option[ShoppingCart]] =
    for {
      _ <- ShoppingCarts[F].create(cartId)
      maybeSc <- ShoppingCarts[F].find(cartId)
      maybeNewSc <- maybeSc.traverse(sc => ShoppingCarts[F].add(sc, product))
    } yield maybeNewSc
}

case class ProgramWithDep[F[_] : Monad](carts: ShoppingCarts[F]) {
  def createAndToCart(product: Product, cartId: String): F[Option[ShoppingCart]] = {
    for {
      _ <- carts.create(cartId)
      maybeSc <- carts.find(cartId)
      maybeNewSc <- maybeSc.traverse(sc => carts.add(sc, product))
    } yield maybeNewSc
  }
}