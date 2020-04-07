package org.openmole.spatialdata.model.spatialinteraction.synthetic

import org.openmole.spatialdata.grid.synthetic.ExpMixtureGridGenerator
import org.openmole.spatialdata.model.spatialinteraction.{DoublyConstrainedSpIntModel, FlowsGenerator, SpatialInteractionModel}
import org.openmole.spatialdata.vector.{FieldGenerator, Point, SpatialField}
import org.openmole.spatialdata.vector.measures.Spatstat
import org.openmole.spatialdata.grid.Implicits._
import org.openmole.spatialdata.utils.math.Matrix.MatrixImplementation
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, SparseMatrix}

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
  * @param gridSize size of the grid
  * @param centers number of centers
  * @param maxOrigin max value of origin kernels
  * @param maxDestination max value of destination kernels
  * @param originRadius origin radius
  * @param destinationRadius destination radius
  * @param originExponent origin exponent
  * @param destinationExponent destination exponent
  * @param costFunction cost function to transform distance mat into spatial cost
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
                                               costFunction: Double => Double
                                               )(implicit mImpl: MatrixImplementation) extends FlowsGenerator {


  /**
    * polycentric flows
    * @param rng
    * @return
    */
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

    def dmat(pi: Seq[Point], pj: Seq[Point]): Matrix = {
      val rawdmat = Spatstat.euclidianDistanceMatrix(pi.toArray,pj.toArray)
      mImpl match {
        case Matrix.Sparse(sImpl) => SparseMatrix(rawdmat.zipWithIndex.map{
          case (row,i) => row.zipWithIndex.map{
            case (d,j) => if (d<3*originRadius) Some((i,j,d)) else None
          }
        }.flatten.filter(_.isDefined).map{_.get},rawdmat.length,rawdmat(0).length)(sImpl)
        case Matrix.Dense(dImpl) => DenseMatrix(rawdmat)(dImpl)
        case Matrix.Empty() => EmptyMatrix()
      }
    }

    def originTransformation(a: Array[Double]): Double = math.pow(a(0),originExponent)
    def destinationTransformation(a: Array[Double]): Double = math.pow(a(0),destinationExponent)
    def flowsFunction: (Seq[Double], Seq[Double], Matrix)=> Matrix = DoublyConstrainedSpIntModel.doublyConstrainedFlows(_,_,_,0.01)
    SyntheticFlowsGenerator(origin,destination,dmat,costFunction,originTransformation,destinationTransformation,flowsFunction).generateFlows
  }


}
