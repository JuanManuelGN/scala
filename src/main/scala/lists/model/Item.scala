package lists.model

sealed abstract class Kind
object Kind {
  final case object O extends Kind
  final case object B extends Kind
}
case class Item(id: Int, kind: Kind)
