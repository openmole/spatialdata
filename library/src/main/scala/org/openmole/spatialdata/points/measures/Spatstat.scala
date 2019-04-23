package org.openmole.spatialdata.points.measures

import com.vividsolutions.jts.geom.GeometryFactory
import org.apache.commons.math3.linear.MatrixUtils
import org.openmole.spatialdata._
import org.openmole.spatialdata.utils.gis.GeometryUtils
import org.openmole.spatialdata.utils.math.Statistics


/**
  * Methods for spatial statistics
  *
  * TODO add standard tests / spatial point processes
  *
  */
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
    val centroid = convexHullCentroid(pi)

    val xcor = pf.map{_._1}
    //val sx = Statistics.std(xcor)
    val sx = xcor.max - xcor.min
    //val mx = Statistics.moment(xcor)
    val mx = centroid._1
    val xnorm = pf.map{case p => (p._1 - mx) / sx}
    //println(xnorm.toSeq)

    val ycor = pf.map{_._2}
    //val sy = Statistics.std(ycor)
    val sy = ycor.max - ycor.min
    //val my = Statistics.moment(ycor)
    val my = centroid._1
    val ynorm = pf.map{case p => (p._2 - my) / sy}

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
    * Average distance between individuals for a set of points
    *  - contrary to a grid, difficult to have a typical radius (points can have a weird ditribution)
    *   -> use the maximal distance
    * @param pi
    * @param x
    * @param weightFunction
    * @param filter
    * @return
    */
  def averageDistance(pi: Array[Point2D],x: Array[Double],filter: Double => Boolean = _ => true): Double = {
    val (pf,xf): (Array[Point2D],Array[Double]) = pi.zip(x).filter{case (p,xx)=>filter(xx)}.unzip
    val n = pf.length
    val dmat = euclidianDistanceMatrix(pf)
    val dmax = dmat.flatten.max
    val xtot = xf.sum
    val xi = MatrixUtils.createRealMatrix(Array.fill(n)(xf)).transpose().getData.flatten.toSeq
    val xj = Array.fill(n)(xf).flatten.toSeq
    xi.zip(xj).zip(dmat.flatten.toSeq).map{case ((xi,xj),dij)=>xi*xj*dij}.sum / (xtot*xtot*dmax)
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





  /**
    * Estimator of Ripley K function
    *  - uses full distance matrix computation
    *
    *  Estimator for a stationary point process is
    *   K(r) = A / (n (n-1)) \sum 1_{d_{ij}< r } e_{ij}
    *
    * with A area of the point cloud, e_{ij} bias correction
    * @param pi point cloud
    * @param radiusSamples number of radius steps at which compute the function
    * @param radiusValues function giving radius values as a function of number of samples - note : radius are normalized to maximal distance such that K(1) = 1/pi
    * @return K(r) as a map
    */
  def ripleyKFunction(pi: Array[Point2D],
                      radiusSamples: Int = 50,
                      radiusValues: Int => Array[Double] = {s => Array.tabulate(s){i => (i+1)*1.0/s}}//,
                     // TODO generic edge correction implementation
                      //edgeCorrection: Map[(Int,Int),Double] = {_ => 1.0}
                     ): Map[Double,Double] = {
    val n = pi.length
    val rvalues = radiusValues(radiusSamples)
    val distmat = euclidianDistanceMatrix(pi)
    val area = GeometryUtils.convexHullArea(pi)
    val dmax = distmat.map{_.max}.max

    rvalues.map{ r =>
      (r,area*distmat.map{_.map{d => if(d/dmax <= r) 1.0 else 0.0}.sum}.sum / (n*(n-1)))
    }.toMap
  }


  /**
    * Pair correlation function is linked to the derivative of Ripley's K
    *
    * @param pi
    * @param radiusSamples
    * @param radiusValues
    * @return
    */
  def pairCorrelationFunction(pi: Array[Point2D],
                              radiusSamples: Int = 50,
                              radiusValues: Int => Array[Double] = {s => Array.tabulate(s){i => (i+1)*1.0/s}}
                             ): Map[Double,Double] = {
    val n = pi.length
    val allripley = ripleyKFunction(pi,radiusSamples,radiusValues)
    val rvalues = allripley.toArray.map{_._1}
    val ripley = allripley.toArray.map{_._2}
    val deltaripley = ripley.tail.zip(ripley.take(n-1)).map{case (kr,krprev) => (kr - krprev)}
    val deltar = rvalues.tail.zip(rvalues.take(n-1)).map{case (r,rprev) => (r - rprev)}
    deltaripley.zip(deltar).zip(rvalues.tail).map{
      case ((dk,dr),r) => (r,dk / (dr * 2*math.Pi*r))
    }.toMap
  }


  /**
    * Inhomogenous K function (non stationary poisson point process with intensity lamba(u))
    *   introduced by Baddeley, A. J., Møller, J., & Waagepetersen, R. (2000). Non‐and semi‐parametric estimation of interaction in inhomogeneous point patterns. Statistica Neerlandica, 54(3), 329-350.
    *   http://sci-hub.tw/https://onlinelibrary.wiley.com/doi/pdf/10.1111/1467-9574.00144
    *   
    * @param pi
    * @return
    */
  def inhomKFunction(pi: Array[Double]): Map[Double,Double] = Map.empty






}




/**
  * A set of summary spatial statistics for a point cloud
  * @param moment1
  * @param moment2
  * @param moment3
  * @param moment4
  * @param unconditionalCount
  * @param conditionalHistogram
  * @param moran
  * @param entropy
  * @param avgDistance
  * @param hierarchy
  * @param spatialMoment01
  * @param spatialMoment10
  * @param spatialMoment11
  * @param spatialMoment20
  * @param spatialMoment02
  */
case class SummarySpatialStatistics(
                                     moment1: Double,
                                     moment2: Double,
                                     moment3: Double,
                                     moment4: Double,
                                     nonCondCount: Double,
                                     conditionalHistogramValues: Array[Double],
                                     conditionalHistogramCounts: Array[Double],
                                     moran: Double,
                                     entropy: Double,
                                     avgDistance: Double,
                                     hierarchy: (Double,Double),
                                     spatialMoment01: Double,
                                     spatialMoment10: Double,
                                     spatialMoment11: Double,
                                     spatialMoment20: Double,
                                     spatialMoment02: Double
                                   ) {
  def toTuple : (Double,Double,Double,Double,Double,Array[Double],Array[Double],Double,Double,Double,Double,Double,Double,Double,Double,Double,Double) =
    (moment1,moment2,moment3,moment4,nonCondCount,conditionalHistogramValues,conditionalHistogramCounts,moran,entropy,avgDistance,hierarchy._1,hierarchy._2,
      spatialMoment01,spatialMoment10,spatialMoment11,spatialMoment20,spatialMoment02)
}

object SummarySpatialStatistics {

  def apply(values: Array[Double],points: Array[Point2D],modeCondition: Double=> Boolean = _ => false,histBreaks: Int = 50): SummarySpatialStatistics = {
    /*
    println("Moment 1 = "+Statistics.moment(values,1,filter = !_.isNaN))
    println("Moment 2 = "+Statistics.moment(values,2,filter = !_.isNaN))
    println("Moment 3 = "+Statistics.moment(values,3,filter = !_.isNaN))
    println("Moment 4 = "+Statistics.moment(values,4,filter = !_.isNaN))
    println("Spatial moment 0 1 = "+Spatstat.spatialMoment(points,values,0,1,filter = !_.isNaN))
    println("Spatial moment 1 0 = "+Spatstat.spatialMoment(points,values,1,0,filter = !_.isNaN))
    println("Spatial moment 1 1 = "+Spatstat.spatialMoment(points,values,1,1,filter = !_.isNaN))
    println("Spatial moment 2 0 = "+Spatstat.spatialMoment(points,values,2,0,filter = !_.isNaN))
    println("Spatial moment 0 2 = "+Spatstat.spatialMoment(points,values,0,2,filter = !_.isNaN))
    */
    val condhist = Statistics.histogram(values.filter{!modeCondition(_)},histBreaks,filter = !_.isNaN, display=true)

    SummarySpatialStatistics(
      Statistics.moment(values,1,filter = !_.isNaN),
      Statistics.moment(values,2,filter = !_.isNaN),
      Statistics.moment(values,3,filter = !_.isNaN),
      Statistics.moment(values,4,filter = !_.isNaN),
      values.filter(modeCondition).size,
      condhist.map{_._1},
      condhist.map{_._2},
      Spatstat.moran(points,values,filter = !_.isNaN),
      Statistics.entropy(values.filter(!_.isNaN)),
      Spatstat.averageDistance(points,values,filter = !_.isNaN),
      Statistics.slope(values.filter(!_.isNaN)),
      Spatstat.spatialMoment(points,values,0,1,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,1,0,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,1,1,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,2,0,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,0,2,filter = !_.isNaN)
    )
  }

}





