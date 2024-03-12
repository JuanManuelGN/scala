package option.intermediate

import cats.syntax.traverse._

import scala.util.Try

trait OptionToTry {
  def toTry[A](op: Option[A], ft: A => Try[A]): Try[Option[A]] = op.traverse(ft)
}
