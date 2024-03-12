package time

import java.time.Instant
import java.time.temporal.ChronoUnit

object HoursBetweenDates extends App {
  /*
    Calcula las horas que hay entre una fecha dada por parámetros en el formato
    ("YYYY-MM-DDThh:mm:ss.sssZ") Should conform to ISO-8601 y ahora
   */
  private def onlyFewHours(ts: String): Long = {
    val from = Instant.parse(ts)
    val to = Instant.now()
    ChronoUnit.HOURS.between(from, to)
  }

  println(s"horas que hay entre dos fechas ${onlyFewHours("2024-03-12T14:45:03.123Z")}")
}
