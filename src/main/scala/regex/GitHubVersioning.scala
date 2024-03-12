package regex

import scala.util.matching.Regex

object GitHubVersioning extends App {
  val versionRegex = "^(v)([0-9]+)\\.([0-9]+)\\.([0-9]+)".r
  val v = "v4.33.1"
  val newTag = v match {
    case versionRegex(v, mayor, minor, patch) => s"$v$mayor.$minor.${patch.toInt + 1}"
  }

  println(newTag)
}
