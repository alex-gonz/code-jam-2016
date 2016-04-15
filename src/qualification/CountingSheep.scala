package qualification

import java.io.{File, PrintWriter}

import scala.io.Source

/**
  * Created by Alex Gonzalez on 4/8/2016.
  */
object CountingSheep {
  val inputFileName = "counting-sheep-in.txt"
  val outputFileName = "counting-sheep-out.txt"
  val writer = new PrintWriter(new File(outputFileName))

  def countSheep(currentNum: Int, initialNum: Int, digitsLeft: Vector[Int]): String = {
    if (currentNum == 0) {
      "INSOMNIA"
    } else {
      val digits = currentNum.toString.toCharArray.map(c => c.toString.toInt)
      val leftover = digitsLeft.filter(!digits.contains(_))
      if (leftover.isEmpty) {
        currentNum.toString
      } else {
        countSheep(currentNum + initialNum, initialNum, leftover)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile(inputFileName).getLines().toList.tail) yield line.toInt
    lines.zipWithIndex.foreach { case (n, index) =>
      writeLine(countSheep(n, n, Vector(0 to 9: _*)), index)
    }


    writer.close()
  }

  def writeLine(result: String, index: Int): Unit = {
    val oneIndexed = index + 1
    writer.write(s"Case #$oneIndexed: $result\n")
  }
}
