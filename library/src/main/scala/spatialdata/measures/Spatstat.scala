

package spatialdata.measures

import spatialdata._
import org.apache.commons.math3.linear._




object Spatstat {


  /**
    * Moran index
    *
    * @param pi set of points
    * @param xi values of field X
    * @return
    */
  def moran(pi: Array[Point2D],x: Array[Double],weightFunction: Array[Point2D]=> Array[Array[Double]] = spatialWeights): Double = {
    val n = pi.length
    val weights: Array[Double] = weightFunction(pi).flatten
    val xavg = x.sum / x.length
    val xx = x.map{_ - xavg}
    val xm: Array[Double] = MatrixUtils.createRealMatrix(Array.fill(n)(xx)).transpose().getData.flatten
    val ym: Array[Double] = Array.fill(n)(xx).flatten
    val cov =  xm.zip(ym).zip(weights).map{case ((xi,xj),wij)=> wij*xi*xj}.sum
    val variance = xx.map{case xi => xi*xi}.sum
    (n*cov) / (weights.sum*variance)
  }


  /**
    * Default spatial weights as w_ij = 1/d_{ij}
    * @param pi
    * @return
    */
  def spatialWeights(pi:Array[Point2D]): Array[Array[Double]] = {
    val dmat = euclidianDistanceMatrix(pi)
    dmat.map{_.map{_ match {case 0.0 => 0.0; case d => 1.0/d}}}
  }

  /**
    * Euclidian distance matrix
    * @param pi
    * @return
    *
    *  FIXME unoptimal, + already coded with great circle dist in geotools
    */
  def euclidianDistanceMatrix(pi: Array[Point2D]): Array[Array[Double]] = {
    val n = pi.length
    val xcoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._1)))
    val ycoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._2)))
    MatrixUtils.createRealMatrix(xcoords.subtract(xcoords.transpose()).getData.map(_.map{case x => x*x})).add(MatrixUtils.createRealMatrix(xcoords.subtract(ycoords.transpose()).getData.map(_.map{case x => x*x}))).getData.map{_.map{math.sqrt(_)}}
  }






}