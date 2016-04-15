package qualification

import java.io.{File, PrintWriter}

import scala.io.Source

object PancakesRevenge {
  val inputFileName = "pancakes-revenge-in.txt"
  val outputFileName = "pancakes-revenge-out.txt"
  val writer = new PrintWriter(new File(outputFileName))

  def countFlips(line: String): Int = {
    val reg = "\\+-".r
    val matchCounts = reg.findAllIn(line).length
    matchCounts * 2 + (if (line.startsWith("-")) 1 else 0)
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile(inputFileName).getLines().toList.tail) yield line
    lines.zipWithIndex.foreach { case (line, zeroIndex) =>
      val result = countFlips(line).toString
      writeLine(result, zeroIndex)
    }

    writer.close()
  }

  def writeLine(result: String, zeroIndex: Int): Unit = {
    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed: $result\n")
  }
}
