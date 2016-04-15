package practice

import java.io.{File, PrintWriter}

import scala.io.Source

object StoreCredit {
  val inputFileName = "store-credit-in.txt"
  val outputFileName = "store-credit-out.txt"

  val writer = new PrintWriter(new File(outputFileName))


  def calculateSolution(credits: Int, itemCosts: Seq[(Int, Int)]): (Int, Int) = {
    @annotation.tailrec
    def go(start: Int, end: Int): (Int, Int) = {
      val startVal = itemCosts(start)._1
      val endVal = itemCosts(end)._1
      if (start >= end) (-1, -1)
      else if (startVal + endVal == credits) (itemCosts(start)._2, itemCosts(end)._2)
      else if (startVal + endVal < credits) go(start + 1, end)
      else go(start, end - 1)
    }
    go(0, itemCosts.length - 1)
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile(inputFileName).getLines().toList.tail) yield line
    val problems = lines.grouped(3)
    problems.zipWithIndex.foreach { case (problem, zeroIndex) =>
      val credits = Integer.parseInt(problem.head)
      val itemCosts = problem(2).split(" ").map(Integer.parseInt)
      val result = calculateSolution(credits, itemCosts.zipWithIndex.sortBy(_._1))
      writeSolution(result, zeroIndex)
    }

    writer.close()
  }
  def writeSolution(result: (Int, Int), zeroIndex: Int): Unit = {
    val first = Math.min(result._1, result._2) + 1
    val second = Math.max(result._1, result._2) + 1
    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed: $first $second\n")
  }
}
