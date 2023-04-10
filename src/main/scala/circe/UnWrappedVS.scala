package circe

import java.time.LocalDate
import io.circe.
object UnWrappedVS extends App {

  final case class Events(events: List[LocalDate]) extends AnyVal

  object Events {
    implicit val decEvents: Decoder[Events] = deriveUnwrappedDecoder
    implicit val encEvents: Encoder[Events] = deriveUnwrappedEncoder
  }

}
