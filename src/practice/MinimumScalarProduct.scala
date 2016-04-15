package practice

import java.io.{File, PrintWriter}

import scala.io.Source

object MinimumScalarProduct {
  val inputFileName = "minimum-scalar-product-in.txt"
  val outputFileName = "minimum-scalar-product-out.txt"

  val writer = new PrintWriter(new File(outputFileName))

  def main(args: Array[String]): Unit = {
    val allLines = for (line <- Source.fromFile(inputFileName).getLines().toList) yield line
    val lines = allLines.tail // ignore the number of problems
    val problems = lines.grouped(3)
    problems.zipWithIndex.foreach { case (problem, zeroIndex) =>
      val first = problem(1).split(" ").map(Integer.parseInt).map(BigInt.apply).sorted.reverse
      val second = problem(2).split(" ").map(Integer.parseInt).map(BigInt.apply).sorted
      val result = first.zip(second).foldRight(BigInt(0)){ (firstAndSecond, acc) =>
        firstAndSecond._1 * firstAndSecond._2 + acc
      }
      writeSolution(result, zeroIndex)
    }

    writer.close()
  }
  def writeSolution(result: BigInt, zeroIndex: Int): Unit = {
    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed: $result\n")
  }
}
