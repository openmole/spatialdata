package org.openmole.spatialdata.model.spatialinteraction.synthetic

import org.openmole.spatialdata.grid.synthetic.ExpMixtureGridGenerator
import org.openmole.spatialdata.model.spatialinteraction.{DoublyConstrainedSpIntModel, FlowsGenerator, SpatialInteractionModel}
import org.openmole.spatialdata.vector.{FieldGenerator, Point, SpatialField}
import org.openmole.spatialdata.vector.measures.Spatstat
import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, SparseMatrix}

import scala.util.Random

/**
  * Basic synthetic configuration for flows in a square world where
  *  - points are cells of a grid
  *  - exponential mixtures with same centers are used for O/D
  *  - O/D fields are resp O_i ^gammaO (resp D_j^gammaD)
  *  - euclidian distance is used
  *  - cost is computed with a given cost function
  *
  *  Center coordinates are not rescaled, so this must be taken into account in the cost function
  *
  * @param gridSize
  * @param centers
  * @param maxOrigin
  * @param maxDestination
  * @param originRadius
  * @param destinationRadius
  * @param originExponent
  * @param destinationExponent
  * @param costFunction
  */
case class PolycentricGridGravityFlowsGenerator(
                                               gridSize: Int,
                                               centers: Int,
                                               maxOrigin: Double,
                                               maxDestination: Double,
                                               originRadius: Double,
                                               destinationRadius: Double,
                                               originExponent: Double,
                                               destinationExponent: Double,
                                               costFunction: Double => Double,
                                               sparse: Boolean = false
                                               ) extends FlowsGenerator {


  override def generateFlows(implicit rng: Random): SpatialInteractionModel = {
    // grids for origin / destination
    val centerCoords: Seq[Point] = Seq.fill(centers)((rng.nextDouble()*gridSize,rng.nextDouble()*gridSize))
    val origin: FieldGenerator[Double] = //() => ExpMixtureGridGenerator (gridSize, centers, maxOrigin, originRadius, false, centerCoords).generateGrid(rng).asSpatialField
    new FieldGenerator[Double] {
      override def generateField(implicit rng: Random): SpatialField[Double] = ExpMixtureGridGenerator (gridSize, centers, maxOrigin, originRadius, false, centerCoords).generateGrid(rng).asSpatialField
    } // the SAM syntax does not work?
    val destination: FieldGenerator[Double] = new FieldGenerator[Double] {
      override def generateField(implicit rng: Random): SpatialField[Double] = ExpMixtureGridGenerator(gridSize,centers,maxDestination,destinationRadius,false,centerCoords).generateGrid(rng).asSpatialField
    }

    def dmat(pi: Seq[Point], pj: Seq[Point]) = {
      val rawdmat = Spatstat.euclidianDistanceMatrix(pi.toArray,pj.toArray)
      if (sparse) SparseMatrix(rawdmat.zipWithIndex.map{case (row,i) => row.zipWithIndex.map{case (d,j) => if (d<3*originRadius) Some((i,j,d)) else None}}.flatten.filter(_.isDefined).map{_.get},rawdmat.length,rawdmat(0).length) else DenseMatrix(rawdmat)
    }

    def originTransformation(a: Array[Double]): Double = math.pow(a(0),originExponent)
    def destinationTransformation(a: Array[Double]): Double = math.pow(a(0),destinationExponent)
    def flowsFunction: (Seq[Double], Seq[Double], Matrix)=> Matrix = DoublyConstrainedSpIntModel.doublyConstrainedFlows(_,_,_,0.01)
    SyntheticFlowsGenerator(origin,destination,dmat,costFunction,originTransformation,destinationTransformation,flowsFunction).generateFlows(rng)
  }


}
