package qualification

import java.io.{File, PrintWriter}

import scala.io.Source

object CoinJam {
  val inputFileName = "coin-jam-in.txt"
  val outputFileName = "coin-jam-out.txt"

  var cache: Map[BigInt, Option[BigInt]] = Map()

  def fillCacheUpTo(coinValue: BigInt, primes: Seq[BigInt], currentValue: BigInt): Option[BigInt] = {
    val maybeDivisor = primes.find(prime => currentValue % prime == 0)
    cache = cache + (currentValue -> maybeDivisor)
    if (coinValue == currentValue) {
      maybeDivisor
    } else {
      maybeDivisor match {
        case None => fillCacheUpTo(coinValue, primes.:+(currentValue), currentValue + 1)
        case _ => fillCacheUpTo(coinValue, primes, currentValue + 1)
      }
    }
  }

  /** Use a sieve + cache to get this */
  def getDivisor(coinValue: BigInt): Option[BigInt] = {
    cache.get(coinValue) match {
      case Some(maybeDivisor) => maybeDivisor
      case None =>
//        val latestCached = Try(cache.keys.max).toOption.getOrElse(1)
        val primes = cache.filter(_._2.isEmpty).keys
        fillCacheUpTo(coinValue, primes.toSeq, coinValue)
    }
  }

  def parseStringAsBigIntWithRadix(binaryCoinString: String, base: Int): BigInt = {
    binaryCoinString.foldLeft(BigInt(0)){
      case (soFar, char) => soFar * base + (if (char == '1') 1 else 0)
    }
  }

  def getDivisorInBases(binaryCoinString: String, basesToCheck: Seq[Int], divisors: Seq[BigInt]): Option[Seq[BigInt]] = {
    if (basesToCheck.isEmpty) {
      Some(divisors)
    } else {
      val base = basesToCheck.head
      val coinValue = parseStringAsBigIntWithRadix(binaryCoinString, base)
      getDivisor(coinValue) match {
        case Some(divisor) => getDivisorInBases(binaryCoinString, basesToCheck.tail, divisors.:+(divisor))
        case None => None
      }
    }
  }

  def generateBinaryString(currentBinaryCoinValue: BigInt, soFar: String = ""): String = {
    if (currentBinaryCoinValue == 0) {
      soFar
    } else {
      val numToAdd = if (currentBinaryCoinValue % 2 == 0) 0 else 1
      val nextString = numToAdd.toString + soFar
      generateBinaryString(currentBinaryCoinValue / 2, nextString)
    }
  }

  def generateCoins(numCoinsNeeded: Int, currentBinaryCoinValue: BigInt, resultSoFar: Seq[(BigInt, Seq[BigInt])] = Seq()): Seq[(BigInt, Seq[BigInt])] = {
    fillCacheUpTo(10000, Seq(), 2)
    if (numCoinsNeeded == resultSoFar.length) {
      resultSoFar
    } else if (currentBinaryCoinValue % 2 == 0) {
      generateCoins(numCoinsNeeded, currentBinaryCoinValue + 1, resultSoFar)
    } else {
      val binaryCoinString = generateBinaryString(currentBinaryCoinValue)
      getDivisorInBases(binaryCoinString, 2 to 10, Seq()) match {
        case Some(divisors) =>
          println(s"found coin: $currentBinaryCoinValue with divisors: $divisors")
          generateCoins(numCoinsNeeded, currentBinaryCoinValue + 1, resultSoFar.:+(currentBinaryCoinValue, divisors))
        case None =>
          generateCoins(numCoinsNeeded, currentBinaryCoinValue + 1, resultSoFar)
      }

    }
  }


  def minCoinValueFor(coinLength: Int): BigInt = {
    (1 until coinLength).foldLeft(BigInt(1)){ case (soFar, _) => soFar * 2 }
  }

  def main(args: Array[String]): Unit = {
    val lines = for (line <- Source.fromFile(inputFileName).getLines().toList.tail) yield line
    lines.zipWithIndex.foreach { case (line, zeroIndex) =>
      val lineInts = line.split(' ').map(_.toInt)
      if (lineInts.length < 2) {
        println(s"Invalid input for line: $line, at line number: ${zeroIndex + 1}")
      } else {
        val result = generateCoins(lineInts(1), minCoinValueFor(lineInts(0)))
        writeSolution(result, zeroIndex)
      }
    }
  }

  def writeSolution(result: Seq[(BigInt, Seq[BigInt])], zeroIndex: Int): Unit = {
    val writer = new PrintWriter(new File(outputFileName))

    val oneIndexed = zeroIndex + 1
    writer.write(s"Case #$oneIndexed:\n")
    result.foreach { coinPlusProof =>
      val binaryCoinString = generateBinaryString(coinPlusProof._1)
      writer.write(s"$binaryCoinString ")
      writer.write(s"${coinPlusProof._2.mkString(" ")}\n")
      val interpretations = (2 to coinPlusProof._2.length + 1).map(base => parseStringAsBigIntWithRadix(binaryCoinString, base))
      coinPlusProof._2.zip(interpretations).foreach { case (proof, interpretation) =>
        if (interpretation % proof != 0) {println(s"binaryCoinString = $binaryCoinString, interpretation = $interpretation, proof = $proof, % = ${interpretation % proof}")}
      }
//      writer.write(s"${interpretations.mkString(" ")}\n")
    }

    writer.close()
  }
}
