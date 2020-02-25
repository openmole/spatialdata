package org.openmole.spatialdata.model.spatialinteraction

import org.openmole.spatialdata.utils.math.{Matrix, SparseMatrix}

trait FittedSpIntModel extends SpatialInteractionModel {
  def fit(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): SpatialInteractionModel => FittedSpIntModel
  def fitted(implicit spMatImpl: SparseMatrix.SparseMatrixImplementation): FittedSpIntModel = fit(spMatImpl)(this)
  def fittedParams: Array[Double]
}


object FittedSpIntModel {


  /**
    * For GLM in scala:
    *  Spark: https://spark.apache.org/docs/2.2.0/api/java/org/apache/spark/ml/regression/GeneralizedLinearRegression.html#GeneralizedLinearRegression--
    *  ScalaGLM which is a breeze wrapper https://darrenjw.github.io/scala-glm/QuickStart.html
    *
    * Comparison of calibration procedures
    * Batty, M., & Mackie, S. (1972). The calibration of gravity, entropy, and related models of spatial interaction. Environment and Planning A, 4(2), 205-233.
    *
    * Calib with admin constraints
    * Batty, M. (1970). Some problems of calibrating the Lowry model. Environment and Planning A, 2(1), 95-114.
    *
    */
  //def poissonFit: SpatialInteractionModel => SpatialInteractionModel = _ => _

}
