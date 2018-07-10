package example

import scala.math.pow
import scala.collection.mutable.ArrayBuffer

object Main extends App {

  val startTime = System.currentTimeMillis()
  val numberList: List[Long] = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  val trueSequence = numberList.map(n => TruePolynomial(n))
  var firstIncorrectTerms = ArrayBuffer[Long]()

  def TruePolynomial(n: Long): Long = {
    val numberList: List[Long] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val sequence = numberList.map(x => if (x % 2 == 0) pow(n, x).toLong else -pow(n, x).toLong)
    sequence.sum
  }

  def FindNextInSequence(terms: Array[Long]): Long = {
    if (terms.length == 1) {
      terms(0)
    } else {
      val processedTerms = terms.zipWithIndex.map{
        case(value, index) =>
          if (index+1 != terms.length) {
            terms(index+1) - terms(index)
          } else {
            ' '
          }
      }.filter(_ != ' ')
      terms.last + FindNextInSequence(processedTerms)
    }
  }

  for (i <- 1 to 10) {
    firstIncorrectTerms += FindNextInSequence(trueSequence.filter(_ <= TruePolynomial(i)).toArray)
    println(s"$i: ${trueSequence(i - 1)} ---> ${firstIncorrectTerms(i - 1)}")
  }

  println
  println(s"The sum of FITs = ${firstIncorrectTerms.sum}")

  val endTime = System.currentTimeMillis()
  println(s"Time taken = ${endTime - startTime} ms")
}
