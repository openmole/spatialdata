

package spatialdata.measures

import spatialdata._
import org.apache.commons.math3.linear._




object Spatstat {


  /**
    * Spatial moment at any order (centered for coordinates) ; normalized by total sum (weights)
    *
    * @param pi
    * @param x
    * @param p
    * @param q
    * @return
    */
  def spatialMoment(pi: Array[Point2D],x: Array[Double],p: Int = 0,q: Int = 0,filter: Double => Boolean = _ => true): Double = {
    val (pf,xf) = pi.zip(x).filter{case (p,xx)=>filter(xx)}.unzip
    val xcor = pf.map{_._1}
    val sx = Statistics.std(xcor)
    val mx = Statistics.moment(xcor)
    val xnorm = pf.map{case p => (p._1 - mx) / sx}
    //println(xnorm.toSeq)
    val ycor = pf.map{_._2}
    val sy = Statistics.std(ycor)
    val my = Statistics.moment(ycor)
    val ynorm = pf.map{case p => (p._2 - my) / sy}
    //println(ynorm.toSeq.map{math.pow(_,q)})
    //println(xf.sum)
    xnorm.zip(ynorm).zip(xf).map{case((xx,yy),f)=>math.pow(xx,p)*math.pow(yy,q)*f}.sum/xf.sum
  }




  /**
    * Moran index for 2d points
    *
    * @param pi set of points
    * @param xi values of field X
    * @param filter optional filter function
    * @return
    */
  def moran(pi: Array[Point2D],x: Array[Double],weightFunction: Array[Point2D]=> Array[Array[Double]] = spatialWeights,filter: Double => Boolean = _ => true): Double = {
    val (pf,xf) = pi.zip(x).filter{case (p,xx)=>filter(xx)}.unzip
    val n = pf.length
    val weights: Array[Double] = weightFunction(pf).flatten
    val xavg = xf.sum / xf.length
    val xx = xf.map{_ - xavg}
    val xm: Array[Double] = MatrixUtils.createRealMatrix(Array.fill(n)(xx)).transpose().getData.flatten
    val ym: Array[Double] = Array.fill(n)(xx).flatten
    val cov =  xm.zip(ym).zip(weights).map{case ((xi,xj),wij)=> wij*xi*xj}.sum
    val variance = xx.map{case xi => xi*xi}.sum
    (n*cov) / (weights.sum*variance)
  }

  /**
    * wrapper for simplified external use
    * @param pi
    * @param x
    */
  def moran(pi: Array[Array[Double]],x: Array[Double]): Double = moran(pi.map{case a => (a(0),a(1)).asInstanceOf[Point2D]},x)


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