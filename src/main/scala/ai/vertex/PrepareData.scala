package ai.vertex

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{FileSystems, Files}
import scala.jdk.CollectionConverters.IteratorHasAsScala

object PrepareData extends App {

  private def writeFile(filename: String, lines: Seq[String]): Unit = {
    val file = new File(filename)
    val bw = new BufferedWriter(new FileWriter(file))
    for (line <- lines) {
      bw.write(line.appendedAll("\n"))
    }
    bw.close()
  }

  val path = "/Users/juanmagn/Documents/GitHub/scala/src/main/scala/ai/vertex/data"

  def readData(fromDir: String): Unit = {
    val lines = Files.list(FileSystems.getDefault.getPath(fromDir))
      .iterator().asScala.map { path =>
        val file = scala.io.Source.fromFile(path.toUri, "ISO-8859-1")
        val text =
          file.getLines.mkString
            .replace("<", "")
            .replace(">", "")
            .replace("/", "")
            .replace("br", "")
            .replace("\"", "")
            .replace("\u0085", " ")
            .replace("\u0096", " ")
        val formatted = if (path.toString.contains("pos")) {
          s"{\"classificationAnnotation\":{\"displayName\":\"positive\"},\"textContent\":\"$text\",\"dataItemResourceLabels\":{\"aiplatform.googleapis.com/ml_use\":\"training\"}}"
        } else
          s"{\"classificationAnnotation\":{\"displayName\":\"negative\"},\"textContent\":\"$text\",\"dataItemResourceLabels\":{\"aiplatform.googleapis.com/ml_use\":\"training\"}}"
        file.close()
        formatted
      }
    writeFile(s"$path/training_dataset_9.jsonl", lines.toList)

  }

  readData("/Users/juanmagn/Documents/GitHub/scala/src/main/scala/ai/vertex/data")
}
