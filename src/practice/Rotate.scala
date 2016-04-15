package practice

import java.io.{File, PrintWriter}

import scala.io.Source

object Rotate {
  val inputFileName = "rotate-in.txt"
  val outputFileName = "rotate-out.txt"

  val writer = new PrintWriter(new File(outputFileName))

  def readProblems(lines: List[String]): List[Problem] = {
    def go(lines: List[String], problems: List[Problem]): List[Problem] = {
      if (lines.isEmpty) {
        problems
      } else {
        val ints = lines.head.split(" ").map(Integer.parseInt)
        val (current, rest) = lines.splitAt(ints(0))
        val board = current.map(_.map(c => if (c == '.') 'E' else c).map(_.toString).map(Square.withName)).map(_.toList)
        go(rest, problems.:+(Problem(ints(1), board)))
      }
    }
    go(lines, List())
  }

  def shiftRight(board: List[List[Square.Value]]): List[List[Square.Value]] = {
    val len = board.head.length

    board.map { row =>
      val lastGood = row.lastIndexWhere(_ != Square.E) + 1
      (1 to len - lastGood).map(_ => Square.E) ++ row.take(lastGood)
    }.map(_.toList)
  }

  def findHorizontalWins(problem: Problem): GameResult.Value = ???

  def findVerticalWins(problem: Problem): GameResult.Value = ???

  def findDiagonalWins(problem: Problem): GameResult.Value = ???

  def findWins(problem: Problem): GameResult.Value = {
    val horizWins = findHorizontalWins(problem)
    if (horizWins == GameResult.Both) {
      GameResult.Both
    } else {
      val verticalWins = findVerticalWins(problem)
      if (verticalWins == GameResult.Both
        || verticalWins == GameResult.Red && horizWins == GameResult.Blue
        || verticalWins == GameResult.Blue && horizWins == GameResult.Red) {
        GameResult.Both
      } else {
        val diagonalWins = findDiagonalWins(problem)
        ???
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val allLines = for (line <- Source.fromFile(inputFileName).getLines().toList) yield line
    val lines = allLines.tail // ignore the number of problems
    val problems = readProblems(lines)
    problems.zipWithIndex.foreach { case (problem, zeroIndex) =>
      val shifted = Problem(problem.k, shiftRight(problem.board))
        val result = findWins(shifted)
//      val first = problem(1).split(" ").map(Integer.parseInt).map(BigInt.apply).sorted.reverse
//      val second = problem(2).split(" ").map(Integer.parseInt).map(BigInt.apply).sorted
//      val result = calculateResult()
//      writeSolution(result, zeroIndex)
    }

    writer.close()
  }
  def writeSolution(result: BigInt, zeroIndex: Int): Unit = {
    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed: $result\n")
  }
}

case class Problem(k: Int, board: List[List[Square.Value]])

object Square extends Enumeration {
  val E, R, B = Value
}

object GameResult extends Enumeration {
  val Neither, Both, Red, Blue = Value
}