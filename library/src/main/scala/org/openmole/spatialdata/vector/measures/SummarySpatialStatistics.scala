package org.openmole.spatialdata.vector.measures

import org.openmole.spatialdata.utils.math.Statistics
import org.openmole.spatialdata.vector.Point


/**
  * A set of summary spatial statistics for a point cloud
  * @param moment1 first moment
  * @param moment2 second moment
  * @param moment3 third moment
  * @param moment4 fourth moment
  * @param nonCondCount number of point not matching a condition
  * @param conditionalHistogramValues values of conditional histogram
  * @param conditionalHistogramCounts counts of conditional histogram
  * @param moran moran
  * @param entropy entropy
  * @param avgDistance average distance
  * @param hierarchy hierarchy
  * @param spatialMoment01 spatial moment 0,1
  * @param spatialMoment10 spatial moment 1,0
  * @param spatialMoment11 spatial moment 1,1
  * @param spatialMoment20 spatial moment 2,0
  * @param spatialMoment02 spatial moment 0,2
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

  def apply(values: Array[Double],points: Array[Point],modeCondition: Double=> Boolean = _ => false,histBreaks: Int = 50): SummarySpatialStatistics = {
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
      Statistics.moment(values,filter = !_.isNaN),
      Statistics.moment(values,2,filter = !_.isNaN),
      Statistics.moment(values,3,filter = !_.isNaN),
      Statistics.moment(values,4,filter = !_.isNaN),
      values.count(modeCondition),
      condhist.map{_._1},
      condhist.map{_._2},
      Spatstat.moran(points,values,filter = !_.isNaN),
      Statistics.entropy(values.filter(!_.isNaN)),
      Spatstat.averageDistance(points,values,filter = !_.isNaN),
      Statistics.slope(values.filter(!_.isNaN)),
      Spatstat.spatialMoment(points,values,q = 1,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,p = 1,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,p = 1, q = 1,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,p = 2,filter = !_.isNaN),
      Spatstat.spatialMoment(points,values,q = 2,filter = !_.isNaN)
    )
  }

}


