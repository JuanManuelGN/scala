package tagless.shopping.algebras

import cats.data.State
import tagless.shopping.domain.ShoppingCart
import tagless.shopping.domain.Product

trait ShoppingCarts[F[_]] {

  def create(id: String): F[Unit]

  def find(id: String): F[Option[ShoppingCart]]

  def add(sc: ShoppingCart, product: Product): F[ShoppingCart]
}

object ShoppingCarts {

  type ShoppingCartRepository = Map[String, ShoppingCart]
  type ScRepoState[A] = State[ShoppingCartRepository, A]

  def apply[F[_]](implicit sc: ShoppingCarts[F]): ShoppingCarts[F] = sc

  implicit object TestShoppingCartInterpreter extends ShoppingCarts[ScRepoState] {

    override def create(id: String): ScRepoState[Unit] =
      State.modify { carts =>
        val shoppingCart = ShoppingCart(id, List())
        carts + (id -> shoppingCart)
      }

    override def find(id: String): ScRepoState[Option[ShoppingCart]] =
      State.inspect { carts =>
        carts.get(id)
      }

    override def add(sc: ShoppingCart, product: Product): ScRepoState[ShoppingCart] =
      State { carts =>
        val products = sc.products
        val updatedCart = sc.copy(products = product :: products)
        (carts + (sc.id -> updatedCart), updatedCart)
      }
  }

  class ShoppingCartWithDependencyInterpreter private(repo: ShoppingCartRepository)
    extends ShoppingCarts[ScRepoState] {
    override def create(id: String): ScRepoState[Unit] =
      State.modify { carts =>
        val shoppingCart = ShoppingCart(id, List())
        carts + (id -> shoppingCart)
      }

    override def find(id: String): ScRepoState[Option[ShoppingCart]] =
      State.inspect { carts =>
        carts.get(id)
      }

    override def add(sc: ShoppingCart, product: Product): ScRepoState[ShoppingCart] =
      State { carts =>
        val products = sc.products
        val updatedCart = sc.copy(products = product :: products)
        (carts + (sc.id -> updatedCart), updatedCart)
      }
  }

  object ShoppingCartWithDependencyInterpreter {
    def make(): ShoppingCartWithDependencyInterpreter = {
      new ShoppingCartWithDependencyInterpreter(repository)
    }

    private val repository: ShoppingCartRepository = Map()
  }
}
