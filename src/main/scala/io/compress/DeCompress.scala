package io.compress

/**
  * Esta clase es usada para descomprimir archivos y escribir su contenido
  */
object DeCompress extends App {

  import java.io.{File, FileInputStream, FileOutputStream}
  import java.util.zip.GZIPInputStream

  /**
    * Decomprime archivos .gz y escribe su contenido
    *
    * @param sourcePath path del archivo .gz
    * @param targetPath destino
    */
  def decompress(sourcePath: String, targetPath: String): Unit = {
    val inputFile = new File(sourcePath)
    val targetFile = new File(targetPath)
    println(s"nombre del archivo = ${inputFile.getName} tamaño del archivo = ${inputFile.length()}")
    val in = new GZIPInputStream(new FileInputStream(inputFile))
    val out = new FileOutputStream(targetFile)
    try {
      val buffer = new Array[Byte](1024)
      var noRead = in.read(buffer)
      while (noRead != -1) {
        out.write(buffer, 0, noRead)
        noRead = in.read(buffer)
      }
    } finally try {
      out.close
    }
    catch {
      case e: Exception => println(s"boom $e")
    }
  }

  decompress("data/inFileGz/hi.parq.gz", "data/outParquetFile/out.parquet")

}
