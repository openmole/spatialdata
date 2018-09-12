
package spatialdata.measures

import spatialdata.utils.math.Convolution

import org.apache.commons.math3.stat.regression.SimpleRegression
import org.apache.commons.math3.util.MathArrays

import scala.math._


/**
  * Morphological indicators for spatial density grids
  *
  * (see Raimbault, J. (2018). Calibration of a density-based model of urban morphogenesis. PloS one, 13(9), e0203516.)
  */
object Morphology {


  /**
    * Rank-size slope
    * Simply estimated by a log-log linear regression
    *
    * TODO add option to better estimate the power law (see Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). Power-law distributions in empirical data. SIAM review, 51(4), 661-703.)
    *
    * @param matrix
    * @return (estimated slope, R2 of the regression)
    */
  def slope(matrix: Array[Array[Double]]): (Double,Double) = {
    def distribution: Array[Double] = matrix.flatten.sorted(Ordering.Double.reverse).filter(_ > 0)
    def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope(), simpleRegression.getRSquare())
  }

  /**
    * Entropy of the distribution
    *
    * @param matrix
    * @return
    */
  def entropy(matrix: Seq[Seq[Double]]) = {
    val totalQuantity = matrix.flatten.sum
    assert(totalQuantity > 0)
    matrix.flatten.map {
      p =>
        val quantityRatio = p/ totalQuantity
        val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log(quantityRatio)
        //assert(!localEntropy.isNaN, s"${quantityRatio} ${math.log(quantityRatio)}")
        localEntropy
    }.sum * (-1 / math.log(matrix.flatten.length))
  }

  /**
    * Mean distance using fast convolution.
    *
    * @param matrix
    * @return
    */
  def distanceMean(matrix: Array[Array[Double]]): Double = {
    val totPop = matrix.flatten.sum
    val dmat = distanceMatrix(2 * matrix.length - 1)
    val conv = Convolution.convolution2D(matrix, dmat)
    math.sqrt(math.Pi) / (matrix.length * totPop * totPop) * MathArrays.ebeMultiply(conv.flatten, matrix.flatten).sum
  }

  /**
    * Distance kernel
    *
    * @param n
    * @return
    */
  def distanceMatrix(n: Int): Array[Array[Double]] = {
    Array.tabulate(n, n) { (i, j) => math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }



  /**
    * Moran index using fast convolution.
    *
    * @param matrix
    * @return
    */
  def moran(matrix: Array[Array[Double]],weightFunction: Array[Array[Double]]=> Array[Array[Double]] = spatialWeights): Double = {
    val n = matrix.length
    val flatConf = matrix.flatten
    val popMean = flatConf.sum / flatConf.length
    val centeredConf = matrix.map { r => r.map { d => d - popMean } }
    val variance = MathArrays.ebeMultiply(centeredConf.flatten, centeredConf.flatten).sum
    val weights = weightFunction(matrix)
    val totWeight = Convolution.convolution2D(Array.fill(n, n) { 1.0 }, weights).flatten.sum
    flatConf.length / (totWeight * variance) * MathArrays.ebeMultiply(centeredConf.flatten, Convolution.convolution2D(centeredConf, weights).flatten).sum
  }

  /**
    * Default spatial weights for Moran
    * @param n
    * @return
    */
  def spatialWeights(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val n:Int = 2 * matrix.length - 1
    Array.tabulate(n, n) { (i, j) => if (i == n / 2 && j == n / 2) 0.0 else 1 / math.sqrt((i - n / 2) * (i - n / 2) + (j - n / 2) * (j - n / 2)) }
  }




  /**
    * Average distance between individuals in the population
    * (direct computation)
    *
    * @param matrix
    * @return
    */
  def distanceMeanDirect(matrix: Array[Array[Double]]): Double = {

    def totalQuantity = matrix.flatten.sum

    def numerator =
      (for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield distance(p1, p2) * c1 * c2).sum

    def normalisation = matrix.length / math.sqrt(math.Pi)

    (numerator / (totalQuantity * totalQuantity)) / normalisation
  }

  def distance(p1: (Int,Int), p2: (Int,Int)): Double = {
    val (i1, j1) = p1
    val (i2, j2) = p2
    val a = i2 - i1
    val b = j2 - j1
    math.sqrt(a * a + b * b)
  }

  def zipWithPosition(m :Array[Array[Double]]): Seq[(Double, (Int,Int))] = {
    for {
      (row, i) <- m.zipWithIndex
      (content, j) <- row.zipWithIndex
    } yield (content,(i, j))
  }


  /**
    * Direct computation of Moran index (in O(N^4))
    * @param matrix
    * @return
    */
  def moranDirect(matrix: Array[Array[Double]]): Double = {
    def flatCells = matrix.flatten
    val totalPop = flatCells.sum
    val averagePop = totalPop / matrix.flatten.length


    def vals =
      for {
        (c1, p1) <- zipWithPosition(matrix)
        (c2, p2) <- zipWithPosition(matrix)
      } yield (decay(p1, p2) * (c1 - averagePop) * (c2 - averagePop),decay(p1, p2))



    def numerator : Double = vals.map{case (n,_)=>n}.sum
    def totalWeight : Double = vals.map{case(_,w)=>w}.sum

    def denominator =
      flatCells.map {
        p =>
          if (p == 0) 0
          else math.pow(p - averagePop.toDouble, 2)
      }.sum

    if (denominator == 0) 0
    else (matrix.flatten.length / totalWeight) * (numerator / denominator)
  }

  def decay(p1:(Int,Int),p2:(Int,Int)) = {
    if (p1==p2) 0.0
    else 1/distance(p1,p2)
  }









}
