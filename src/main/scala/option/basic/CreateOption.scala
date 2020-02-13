package option.basic

import java.net.URI

import scala.util.Try

/**
  * En esta clase se van a explorar las diferentes formas de crear Option[A]
  */
object CreateOption extends App {

  val nilString = Nil
  val emptyString = ""
  val s = "im String"
  val saludo = "hola"
  val despedida = "adios"
  val boom = "boom"

  val nilOption = Option(nilString)
  println(s"Se ha creado un Option con Option(Nil) = ${nilOption}")
  /**
    * Como se puede observar se crea como un Some(List()) y nuestra intención inicial era que se
    * creara como un None sin realizar comprobaciones tipo if nilString != ""....
    */

  /**
    * Ahora vamos a crear un Option con usando un filter para obtener None cuando el String sea
    * la cadena vacía o Nil y Some cuando la cadena esté bien definida
    */
  val emptyOption = Option(emptyString).filter(_.nonEmpty)
  println("Se ha creado un Option con Option(\"\").filter(_.nonEmpty) = " +
    s"${emptyOption}")

  val nilOptionFilter = Option(nilString).filter(_.nonEmpty)
  println(s"Se ha creado un Option con Option(Nil).filter(_.nonEmpty) = ${nilOptionFilter}")

  val sOption = Option(s).filter(_.nonEmpty)
  println("Se ha creado un Option con Option(\"im String\").filter(_.nonEmpty) = " +
    s"${sOption}")

  case class Url(protocol: String, host: String, path: String)
  def getUrl(uri: java.net.URI): Option[Url] =
    for {
      protocol <- Option(uri.getScheme)
      host <- Option(uri.getHost)
      path <- Option(uri.getRawPath)
    }
      yield Url(protocol, host, path)

  val uriMalFormet = new URI("")
  val uriOk = new URI("https://www.geeksforgeeks.org/url-class-java-examples/")
  println(s"Se obtiene un Option para la clase URL con una uri mal formada = ${getUrl(uriMalFormet)}")
  println(s"Se obtiene un Option para la clase URL con una uri bien formada = ${getUrl(uriOk)}")

  /** Creación de un Option a raíz de un cómputo */

  println("Creación de un Option a raíz de un cómputo")
  val optComputeSome = Try(runCompute(saludo)).toOption
  println("Se ha creado un Option con Try(runCompute(\"hola\")).toOption = " +
    s"${optComputeSome}")

  val optComputeNone = Try(runCompute(despedida)).toOption

  println("Se ha creado un Option con Try(runCompute(\"adiós\")).toOption = " +
    s"${optComputeNone}")

  def runCompute(s: String): String = {
    s match {
      case "hola" => "que pasa"
      case _ => throw new Exception(boom)
    }
  }
  /***********************************************/
}
