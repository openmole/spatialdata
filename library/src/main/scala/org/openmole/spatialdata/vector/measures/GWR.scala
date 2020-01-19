package org.openmole.spatialdata.vector.measures

import org.apache.commons.math3.stat.regression._
import org.apache.commons.math3.linear._
import org.openmole.spatialdata.vector.Point


object GWR {


  case class GWRResult(
                        estimatedParameters: Array[Array[Double]]
                      )


  /**
    *
    * multivariate reg https://commons.apache.org/proper/commons-math/javadocs/api-3.0/org/apache/commons/math3/stat/regression/OLSMultipleLinearRegression.html#newSampleData(double[],%20double[][])
    *
    * GWModel package in R https://arxiv.org/pdf/1306.0413.pdf
    *
    * @param yvalues
    * @param xvalues
    * @param points
    * @return
    */
  def basicGWR(yvalues: Array[Double],xvalues: Array[Array[Double]], points: Array[Point], bandwidth: Double, kernel: (Double,Double) => Double): GWRResult = {
    //val reg = new OLSMultipleLinearRegression()
    //reg.newSampleData(yvalues,xvalues)
    //GWRResult(Array(reg.estimateRegressionParameters()))
    GWRResult(Array.empty)
  }

  /**
    * regression in itself
    *
    * beta = [t(W)t(X)X]^-1 * t(X) [wi yi]
    *  + conf ints ?
    *
    * @param yvalues
    * @param xvalues
    * @param weights
    */
  def gwr(yvalues: Array[Double],xvalues: Array[Array[Double]], weights: Array[Double]) = {
    MatrixUtils.createRealMatrix(xvalues)
  }



}
