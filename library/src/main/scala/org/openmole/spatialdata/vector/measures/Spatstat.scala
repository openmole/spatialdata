package org.openmole.spatialdata.vector.measures

import com.vividsolutions.jts.geom.GeometryFactory
import org.apache.commons.math3.linear.MatrixUtils
import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.utils.gis.GeometryUtils
import org.openmole.spatialdata.utils.math.Statistics


/**
  * Methods for spatial statistics
  *
  * TODO add standard tests / spatial point processes
  *
  *  TODO Implement Rey, S. J., & Janikas, M. V. (2006). STARS: space–time analysis of regional systems. Geographical analysis, 38(1), 67-86.
  *
  */
object Spatstat {


  /**
    * Spatial moment at any order (centered for coordinates) ; normalized by total sum (weights)
    *
    * @param pi points
    * @param x field values
    * @param p order on x
    * @param q order on y
    * @param filter optional filter on field values
    * @return
    */
  def spatialMoment(pi: Array[Point],x: Array[Double],p: Int = 0,q: Int = 0,filter: Double => Boolean = _ => true): Double = {
    val (pf,xf) = pi.zip(x).filter{case (p,xx)=>filter(xx)}.unzip
    val centroid = GeometryUtils.convexHullCentroid(pi)

    val xcor = pf.map{_._1}
    //val sx = Statistics.std(xcor)
    val sx = xcor.max(Ordering.Double.TotalOrdering) - xcor.min(Ordering.Double.TotalOrdering)
    //val mx = Statistics.moment(xcor)
    val mx = centroid._1
    val xnorm = pf.map{case p => (p._1 - mx) / sx}
    //println(xnorm.toSeq)

    val ycor = pf.map{_._2}
    //val sy = Statistics.std(ycor)
    val sy = ycor.max(Ordering.Double.TotalOrdering) - ycor.min(Ordering.Double.TotalOrdering)
    //val my = Statistics.moment(ycor)
    val my = centroid._1
    val ynorm = pf.map{case p => (p._2 - my) / sy}

    xnorm.zip(ynorm).zip(xf).map{case((xx,yy),f)=>math.pow(xx,p)*math.pow(yy,q)*f}.sum/xf.sum
  }




  /**
    * Moran index for 2d points
    *
    * @param pi set of points
    * @param x values of field X
    * @param filter optional filter function
    * @return
    */
  def moran(pi: Array[Point],x: Array[Double],weightFunction: Array[Point]=> Array[Array[Double]] = spatialWeights,filter: Double => Boolean = _ => true): Double = {
    val (pf,xf) = pi.zip(x).filter{case (_,xx)=>filter(xx)}.unzip
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
  def moran(pi: Array[Array[Double]],x: Array[Double]): Double = moran(pi.map{case a => (a(0),a(1))},x)


  /**
    * Average distance between individuals for a set of points
    *  - contrary to a grid, difficult to have a typical radius (points can have a weird ditribution)
    *   -> use the maximal distance
    * @param pi points
    * @param x field values
    * @param filter optional filter
    * @return
    */
  def averageDistance(pi: Array[Point],x: Array[Double],filter: Double => Boolean = _ => true): Double = {
    val (pf,xf): (Array[Point],Array[Double]) = pi.zip(x).filter{case (p,xx)=>filter(xx)}.unzip
    val n = pf.length
    val dmat = euclidianDistanceMatrix(pf)
    val dmax = dmat.flatten.max(Ordering.Double.TotalOrdering)
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
  def spatialWeights(pi:Array[Point]): Array[Array[Double]] = {
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
  def euclidianDistanceMatrix(pi: Array[Point]): Array[Array[Double]] = {
    val n = pi.length
    val xcoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._1)))
    val ycoords = MatrixUtils.createRealMatrix(Array.fill(n)(pi.map(_._2)))
    MatrixUtils.createRealMatrix(xcoords.subtract(xcoords.transpose()).getData.map(_.map{case x => x*x})).add(MatrixUtils.createRealMatrix(ycoords.subtract(ycoords.transpose()).getData.map(_.map{case x => x*x}))).getData.map{_.map{math.sqrt(_)}}
  }

  def euclidianDistanceMatrix(pi: Array[Point], pj: Array[Point]): Array[Array[Double]] = {
    val (n,p) = (pi.length, pj.length)
    val xj = MatrixUtils.createRealMatrix(Array.fill(n)(pj.map(_._1)))
    val xi = MatrixUtils.createRealMatrix(Array.fill(p)(pi.map(_._1))).transpose()
    val yj = MatrixUtils.createRealMatrix(Array.fill(n)(pj.map(_._2)))
    val yi = MatrixUtils.createRealMatrix(Array.fill(p)(pi.map(_._2))).transpose()
    MatrixUtils.createRealMatrix(xi.subtract(xj).getData.map(_.map{case x => x*x})).
      add(MatrixUtils.createRealMatrix(yi.subtract(yj.transpose()).getData.map(_.map{case x => x*x}))).getData.map{_.map{math.sqrt}}
  }




  /**
    * Estimator of Ripley K function
    *  - uses full distance matrix computation
    *
    *  Estimator for a stationary point process is
    *   K(r) = A / (n (n-1)) \sum 1_{d_{ij}< r } e_{ij}
    *
    * with A area of the point cloud, e_{ij} bias correction
    *
    *  TODO for scalar summary of ripley, instead of piecewise linear, use integral ?
    *
    * @param pi point cloud
    * @param radiusSamples number of radius steps at which compute the function
    * @param radiusValues function giving radius values as a function of number of samples - note : radius are normalized to maximal distance such that K(1) = 1/pi
    * @return K(r) as a map
    */
  def ripleyKFunction(pi: Array[Point],
                      radiusSamples: Int = 50,
                      radiusValues: Int => Array[Double] = {s => Array.tabulate(s){i => (i+1)*1.0/s}}//,
                     // TODO generic edge correction implementation
                      //edgeCorrection: Map[(Int,Int),Double] = {_ => 1.0}
                     ): Map[Double,Double] = {
    val n = pi.length
    val rvalues = radiusValues(radiusSamples)
    val distmat = euclidianDistanceMatrix(pi)
    val area = GeometryUtils.convexHullArea(pi)
    val dmax = distmat.map{_.max(Ordering.Double.TotalOrdering)}.max(Ordering.Double.TotalOrdering)

    rvalues.map{ r =>
      (r,area*distmat.map{_.filter{d => d/dmax <= r}.map{_ => 1.0}.sum}.sum / (n*(n-1)))
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
  def pairCorrelationFunction(pi: Array[Point],
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


