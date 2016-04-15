package qualification

import java.io.{File, PrintWriter}

import scala.io.Source

object Fractiles {
  val inputFileName = "fractiles-in.txt"
  val outputFileName = "fractiles-out.txt"
  val writer = new PrintWriter(new File(outputFileName))

  object Tile extends Enumeration {
    val L, G = Value
  }

  def generateInitial(initialLength: Int): Seq[Seq[Tile.Value]] = {
    val allL = (1 to initialLength).map(_ => Tile.L)
    (0 until initialLength).map(index => allL.patch(index, Seq(Tile.G), 1))
  }

  def doIteration(current: Seq[Tile.Value], initial: Seq[Tile.Value]): Seq[Tile.Value] = {
    current.flatMap {
      case Tile.L => initial
      case Tile.G => (1 to initial.length).map(_ => Tile.G)
    }
  }

  def generatePossibilities(initialLength: Int, numIterations: Int, maxChecks: Int) = {
    val initialPossibilities = generateInitial(initialLength)
    println(s"Generating possibilities for: len = $initialLength, iter = $numIterations:")
    println(s"Starting with $initialPossibilities")
    val result = (1 until numIterations).foldRight(initialPossibilities.zip(initialPossibilities)) { case (_, possibilities) =>
      possibilities.map { case (current, initial) =>
        (doIteration(current, initial), initial)
      }
    }
    result.map(_._1).foreach(println)
    val earlierHalf = result.zipWithIndex.filter { case (_, index) => index < 3 }.map(_._1)
//    val middleHalf = result.zipWithIndex.filter { case (_, index) => index < 4 && index >= 2 }.map(_._1)
    val latterHalf = result.zipWithIndex.filter { case (_, index) => index >= 3 }.map(_._1)

    Seq(earlierHalf, latterHalf).foreach { half =>
      val matches = (0 until math.pow(initialLength, numIterations).toInt).map { pos =>
        if (half.map(_._1).forall(seq => seq(pos) == Tile.G)) {
          Some(pos + 1)
        } else {
          None
        }
      }.filter(_.isDefined).map(_.get)
      println(s"found matches as positions: $matches")
    }
  }

  def calculateGroup(len: Int, iter: Int, start: Int, end: Int): BigInt = {
    val groupLength = BigInt(len).pow(iter - 1)
    val startGroup = start
    groupLength * startGroup + (iter - 2 ) * (end - 1)* BigInt(len).pow(iter - 2) + end + 1
  }

  def calculateGroups(initialLength: Int, numIterations: Int): Option[Seq[BigInt]] = {
    val numGroups = (initialLength.toDouble / numIterations).ceil.toInt
    val places = (1 to numGroups).map(pos => calculateGroup(initialLength, numIterations, (pos - 1) * numIterations, pos * numIterations - 1))
    Some(places)
  }

  def generateSolution(initialLength: Int, numIterations: Int, maxChecks: Int): Option[Seq[BigInt]] = {
    if ((initialLength.toDouble / numIterations).ceil > maxChecks) {
      None
    } else if (initialLength == 1) {
      Some(Seq(1))
    } else if (initialLength <= numIterations) {
      val n = BigInt(initialLength)
      val onlyCheck = (n.pow(initialLength) - n) / (n - 1).pow(2) // https://oeis.org/A058128
      Some(Seq(onlyCheck))
    } else if (maxChecks == initialLength) {
      Some((1 to initialLength).map(BigInt.apply))
    } else {
      calculateGroups(initialLength, numIterations)
    }
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile(inputFileName).getLines().toList.tail) yield line
    lines.zipWithIndex.foreach { case (line, zeroIndex) =>
      val lineInts = line.split(' ').map(_.toInt)
      if (lineInts.length < 3) {
        println(s"Invalid input for line: $line, at line number: ${zeroIndex + 2}")
      } else {
        val initialLength = lineInts(0)
        val numIterations = lineInts(1)
        val maxChecks = lineInts(2)

//        generatePossibilities(initialLength, numIterations, maxChecks)
        val result = generateSolution(initialLength, numIterations, maxChecks)
        writeSolution(result, zeroIndex)
      }
    }

    writer.close()
  }

  def writeSolution(result: Option[Seq[BigInt]], zeroIndex: Int): Unit = {
    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed: ")
    val resultString = result match {
      case Some(checks) => checks.mkString(" ")
      case None => "IMPOSSIBLE"
    }
    writer.write(s"$resultString\n")
  }
}
