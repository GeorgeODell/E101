package example

import scala.math.pow
import scala.collection.mutable.ArrayBuffer

/**
  * Created by louisdmannevy on 09/07/18.
  */

object Main extends App {

  var OptimalPolynomials = ArrayBuffer[Long]()
  val numberList: List[Long] = List.range(1, 20)

  def TruePolynomial(n: Long): Long = {
    val numberList: List[Long] = List.range(0, 11)
    val sequence = numberList.map(x => if (x % 2 == 0) pow(n, x).toLong else -pow(n, x).toLong)
    sequence.sum
  }

  def FindOptimalSequence(terms: Array[Int]): Int = {
    if (terms.length == 1) {
      terms(0)
    } else {
      val preprocessedTerms = terms.zipWithIndex.map{
        case(value, index) =>
          if (index+1 != terms.length) {
            terms(index+1) - terms(index)
          } else {
            0
          }
      }
      val processedTerms = preprocessedTerms.filter(_ != 0)
      terms.last + FindOptimalSequence(processedTerms)
    }
  }

  numberList.zipWithIndex.foreach{case(e, i) => println(s"${i+1}: ${TruePolynomial(e)}")}

  println(FindOptimalSequence(Array(1, 8, 27)))


}
