
package spatialdata.measures

import org.apache.commons.math3.stat.regression.SimpleRegression
import org.dianahep.histogrammar._

import scala.math.{Ordering, log}

object Statistics {


  /**
    * Unbiased moment at a given order
    * @param x
    * @param order
    * @param weighting optional array of weights (will be renormalized if not already)
    * @param filter filtering function
    * @return
    */
  def moment(x: Array[Double],order: Int = 1,weighting : Array[Double]=Array.empty,filter: Double => Boolean = _ => true): Double = {
    val w: Array[Double] = x.zip(weighting).filter{case (x,_)=>filter(x)}.map{case (x,w)=> w} match {
      case a if (a.size==0) => Array.fill(x.size){1.0/x.size}
      case a if (a.sum != 1.0) => {val s = a.sum; a.map{_/s}}
      case a => a
    }
    x.filter(filter).zip(w).map{case (x,w) => w*math.pow(x,order.toDouble)}.sum
  }



  /**
    * Histogram
    * @param x
    * @param breaks
    * @return
    */
  def histogram(x: Array[Double],breaks: Int,filter: Double => Boolean = _ => true,display:Boolean = false): Array[(Double,Double)] = {
    val xx = x.filter(filter)
    val hist = Bin(breaks,xx.min-1e-6,xx.max+1e-6,{d: Double=>d})
    for (d <- xx) {hist.fill(d)}

    if(display) {
      println("min = "+xx.min+" ; max ="+xx.max)
      import org.dianahep.histogrammar.ascii._
      println(hist.ascii)
    }

    val xstep = (xx.max - xx.min) / breaks
    Array.tabulate(breaks){case i => xstep / 2 + i*xstep}.zip(hist.values.map{_.asInstanceOf[Counting].entries})
  }

  /**
    * biased estimator of the std
    * @param x
    * @return
    */
  def std(x: Array[Double]): Double = {
    val ex = moment(x)
    math.sqrt(moment(x,2) - ex*ex)
  }



  /**
    * Rank-size slope
    * Simply estimated by a log-log linear regression
    *
    * TODO add option to better estimate the power law (see Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). Power-law distributions in empirical data. SIAM review, 51(4), 661-703.)
    *
    * @param matrix
    * @return (estimated slope, R2 of the regression)
    */
  def slope(matrix: Array[Array[Double]]): (Double,Double) = slope(matrix.flatten)

  def slope(matrix: Seq[Seq[Double]]): (Double,Double) = slope(matrix.flatten.toArray)

  def slope(values: Array[Double]): (Double,Double) = {
    def distribution: Array[Double] = values.sorted(Ordering.Double.reverse).filter(_ > 0)
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
  def entropy(matrix: Array[Array[Double]]): Double = entropy(matrix.flatten)

  def entropy(matrix: Seq[Seq[Double]]): Double = entropy(matrix.flatten.toArray)


  /**
    *
    * @param values
    * @return
    */
  def entropy(values: Array[Double]): Double = {
    val totalQuantity = values.sum
    //assert(totalQuantity > 0)

      totalQuantity match {
        case 0.0 => 0.0
        case _ =>
           values.map {p =>
              val quantityRatio = p / totalQuantity
              val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log (quantityRatio)
             //assert(!localEntropy.isNaN, s"${quantityRatio} ${math.log(quantityRatio)}")
              localEntropy
           }.sum * (- 1 / math.log (values.length) )
      }
  }




}

