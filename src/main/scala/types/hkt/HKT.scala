package types.hkt


/** Que es un Higher-Kinded Type de Scala?
  * https://emanuelpeg.blogspot.com/2024/05/que-es-un-higher-kinded-type-de-scala.html?m=1
  */


// Definición de un Higher-Kinded Type (HKT) F[_]
trait Container[F[_]] {

  def put[A](value: A): F[A]

  def get[A](container: F[A]): A

}

// Implementación de Container para List

object ListContainer extends Container[List] {

  def put[A](value: A): List[A] = List(value)

  def get[A](container: List[A]): A = container.head

}

// Implementación de Container para Option

object OptionContainer extends Container[Option] {

  def put[A](value: A): Option[A] = Some(value)

  def get[A](container: Option[A]): A = container.getOrElse(throw new NoSuchElementException("Empty container"))

}

object Main {

  def main(args: Array[String]): Unit = {

    // Uso de ListContainer

    val list = ListContainer.put(42)

    println("Value in list: " + ListContainer.get(list)) // Imprime: Value in list: 42



    // Uso de OptionContainer

    val option = OptionContainer.put(42)

    println("Value in option: " + OptionContainer.get(option)) // Imprime: Value in option: 42

  }

}
