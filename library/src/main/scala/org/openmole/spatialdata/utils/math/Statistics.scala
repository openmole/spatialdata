package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.stat.regression.SimpleRegression
//import org.dianahep.histogrammar._
//import org.dianahep.histogrammar.ascii._

import scala.math.{Ordering, log}

/**
  * Statistics utility
  *
  * Future work:
  *  - on the estimation of power laws, implement
  *    Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). Power-law distributions in empirical data. SIAM review, 51(4), 661-703.
  *
  *
  */
object Statistics {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering


  /**
    * Distribute a variable following a rank size
    * @param size number of elements
    * @param alpha slope
    * @param pmax max value
    * @return
    */
  def rankSizeDistribution(size: Int,alpha: Double, pmax: Double): Vector[Double] =
    (1 to size by 1).map{i => pmax*math.pow(1.0/i.toDouble,alpha)}.toVector


  /**
    * Unbiased moment at a given order
    * @param x values
    * @param order order
    * @param weighting optional array of weights (will be renormalized if not already)
    * @param filter filtering function
    * @return
    */
  def moment(x: Array[Double],order: Int = 1,weighting : Array[Double]=Array.empty,filter: Double => Boolean = _ => true): Double = {
    val w: Array[Double] = x.zip(weighting).filter{case (xx,_)=>filter(xx)}.map{case (_,ww)=> ww} match {
      case a if a.length==0 => Array.fill(x.length){1.0/x.length}
      case a if a.sum != 1.0 =>
        val s = a.sum
        a.map{_/s}
      case a => a
    }
    x.filter(filter).zip(w).map{case (xx,ww) => ww*math.pow(xx,order.toDouble)}.sum
  }



  /**
    * Histogram
    * @param x values
    * @param breaks number of breaks
    * @return
    */
  def histogram(x: Array[Double],breaks: Int,filter: Double => Boolean = _ => true,display:Boolean = false): Array[(Double,Double)] = {
    val xx = x.filter(filter)
    val counts = Array.fill(breaks)(0.0)
    //val hist = Bin(breaks,xx.min-1e-6,xx.max+1e-6,{d: Double=>d})
    //for (d <- xx) {hist.fill(d)}
    val ampl = xx.max - xx.min
    val mi = xx.min
    for (d <- xx) {
      val bin = ((d - mi) * (breaks - 1) / ampl).toInt
      counts(bin) = counts(bin) + 1.0
    }

    /*
    if(display) {
      println("min = "+xx.min+" ; max ="+xx.max)
      println(hist.ascii)
    }*/

    val xstep = ampl / breaks
    //Array.tabulate(breaks){case i => xstep / 2 + i*xstep}.zip(hist.values.map{_.asInstanceOf[Counting].entries})
    Array.tabulate(breaks)( i => xstep / 2 + i*xstep).zip(counts)
  }

  /**
    * biased estimator of the std
    * @param x values
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
    * Future work:
    *  add option to better estimate the power law (see Clauset, A., Shalizi, C. R., & Newman, M. E. (2009). Power-law distributions in empirical data. SIAM review, 51(4), 661-703.)
    *
    * @param matrix values
    * @return (estimated slope, R2 of the regression)
    */
  def slope(matrix: Array[Array[Double]]): (Double,Double) = slope(matrix.flatten)

  def slope(matrix: Seq[Seq[Double]]): (Double,Double) = slope(matrix.flatten.toArray)

  def slope(values: Array[Double]): (Double,Double) = {
    def distribution: Array[Double] = values.filter(_ > 0).sorted(Ordering.Double.TotalOrdering.reverse)
    def distributionLog: Array[Array[Double]] = distribution.zipWithIndex.map { case (q, i) => Array(log(i + 1), log(q)) }
    val simpleRegression = new SimpleRegression(true)
    simpleRegression.addData(distributionLog)
    (simpleRegression.getSlope, simpleRegression.getRSquare)
  }



  /**
    * Entropy of the distribution
    *
    * @param matrix values
    * @return
    */
  def entropy(matrix: Array[Array[Double]]): Double = entropy(matrix.flatten)

  def entropy(matrix: Seq[Seq[Double]]): Double = entropy(matrix.flatten.toArray)


  /**
    * Entropy of a stat distrib
    * @param values values
    * @return
    */
  def entropy(values: Array[Double]): Double = {
    val x = values.map{d => if (d.isNaN) 0.0 else d}
    val totalQuantity = x.sum
    //assert(totalQuantity > 0)

      totalQuantity match {
        case 0.0 => 0.0
        case _ =>
          x.map {p =>
              val quantityRatio = p / totalQuantity
              val localEntropy = if (quantityRatio == 0.0) 0.0 else quantityRatio * math.log (quantityRatio)
              localEntropy
           }.sum * (- 1 / math.log (x.length) )
      }
  }


  //
  /*
  def conditionalExpectancies(initialValues: Vector[Double],
                              initialState: Vector[Vector[Double]],
                              valToState: (Vector[Double],Vector[Vector[Double]])=>Vector[Vector[Double]],
                              probabilitiesFunction: (Vector[Vector[Double]],(Int => Vector[Double])) => (Int => Vector[Double]) = discreteChoicesProba,
                              increments: Vector[Double]
                             ): Vector[Vector[Double]] = {
    def cond0(prevState: Vector)
  }
  */

/*
  def discreteChoicesProbaTime(state: Vector[Vector[Double]],beta: Int => Vector[Double]): Int => Vector[Double] = {
    t =>
      Matrix.toFlatVector(Matrix.fromVector(state).multiply(Matrix.fromColumnVector(beta(t))))
  }*/

  def discreteChoicesProbaTime(state: Array[Array[Double]],beta: Int => Array[Double]): Int => Array[Double] = {
    _ => Array.empty
      //Matrix.toFlatArray(Matrix.fromArray(state).multiply(Matrix.fromColumnArray(beta(t))))
  }


  //def discreteChoicesProba(state: Vector[Vector[Double]], beta: Vector[Double]): Vector[Double] = discreteChoicesProbaTime(state,{_ => beta})(0)

  def discreteChoicesProba(state: Array[Array[Double]], beta: Array[Double]): Array[Double] = discreteChoicesProbaTime(state,{_ => beta})(0)


  /**
    * L2 discrepancy
    * @param points points
    * @return
    */
  def discrepancyL2(points: Array[Array[Double]]): Double = {
    assert(nonEmptyPoints(points),"Discrepancy must be computed on non empty vectors in the same space")
    val (n,d) = (points.length,points(0).length)
    // rescale the data
    val (mins,maxs) = points.transpose.map{col => (col.min,col.max)}.unzip
    val npoints = points.map{_.zip(mins).zip(maxs).map{case ((x,mi),ma) => (x - mi) / (ma - mi)}}
    // s1 = sum_i (Prod_j (x_ij * (1 - x_ij)))
    val s1 = npoints.map{_.map{xij => xij*(1 - xij)}.product}.sum
    // s2 = sum_{i,k} {Prod_d (1 - max(X_ij, X_kj)) * min(X_ij, X_kj)}
    val s2 = points.flatMap{xi => xi.zip(points).map{case (xij,xk) => xk.map{xkj => (1 - math.max(xij,xkj))*math.min(xij,xkj)}.product}}.sum
    math.sqrt(math.pow(12.0,-d) - (((2^(1 - d))/n) * s1) + ((1/n^2) * s2))
  }






}
