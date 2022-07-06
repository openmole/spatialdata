package org.openmole.spatialdata.test

import org.openmole.spatialdata.model.spatialinteraction.{SinglyConstrainedSpIntModel, SpatialInteractionModel}
import org.openmole.spatialdata.model.spatialinteraction.synthetic.PolycentricGridGravityFlowsGenerator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.math.SparseMatrix.SparseMatrixImplementation
import org.openmole.spatialdata.vector.SpatialField
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, EmptyMatrix, Matrix, SparseMatrix}

import scala.util.Random

object TestSpatialInteraction {

  /**
    * test time perf with random matrices
    */
  def testFitSinglyConstrainedRandom(): Unit = {

    implicit val rng: Random = new Random
    implicit val spMatImpl: SparseMatrixImplementation = SparseMatrix.SparseBreeze()

    val (n,p) = (8436,8436)

    val flows = utils.timerLog[Matrix](SparseMatrix.randomSparseMatrix(n, p, 0.03),"random flows")
    //val dmat = SparseMatrix.randomSparseMatrix(n, p, 0.43)
    val dmat = utils.timerLog[Matrix](SparseMatrix.randomSparseMatrix(n, p, 0.05), "random dmat")
    val origin = Seq.fill(n)(rng.nextDouble())
    val destination = Seq.fill(n)(rng.nextDouble())
    val originfield: SpatialField[Double]=origin.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    val destinationfield: SpatialField[Double]=destination.zipWithIndex.map{case (s,i) => ((i.toDouble,0.0),Array(s))}.toMap
    utils.log(s"total flows = ${flows.sum}")
    val model = SinglyConstrainedSpIntModel(
      observedFlows = flows,
      distanceWeightsMatrix = Some(dmat),
      originValues = originfield,
      destinationValues = destinationfield
    )
    val _ = model.fitted
  }


  /**
    * quant data
    */
  def testFlowData(): Unit = {
    implicit val spMatImpl: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()
    implicit val dImpl: DenseMatrix.DenseMatrixImplementation = DenseMatrix.DenseBreeze()
    val flowspath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/TObs_1.csv"
    val dmatpath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/dis_roads_min.csv"
    val flowmat = utils.timerLog[String,SparseMatrix](s => CSV.readSparseMat(s),flowspath,"read flows")
    val dmat = utils.timerLog[String,DenseMatrix](s=>DenseMatrix(CSV.readMat(s)),dmatpath,"read dmat")
    println(s"flows mat: ${flowmat.nrows}x${flowmat.ncols}; nentries = ${flowmat.nentries}")
    println(s"dmat: ${dmat.nrows}x${dmat.nrows}")
    val flowsparsity = utils.timerLog[Double](flowmat.values.flatten.count(_>0.0).toDouble/(flowmat.nrows.toDouble*flowmat.ncols.toDouble),"flow sparsity")
    val dmatsparsity = utils.timerLog[Double](dmat.map{d => math.exp(-d/60)}.values.flatten.count(_>0.1).toDouble/(dmat.nrows.toDouble*dmat.ncols.toDouble),"dmat sparsity")
    println(s"flow sparsity: $flowsparsity ; dmatsparsity: $dmatsparsity")
  }


  /**
    *
    */
  def testFitSinglyConstrainedSyntheticFlows(): Unit = {
    implicit val rng: Random = new Random
    implicit val mImpl: Matrix.MatrixImplementation = Matrix.Sparse(SparseMatrix.SparseBreeze())
    implicit val spImpl: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()

    val syntheticFlows = PolycentricGridGravityFlowsGenerator(
      gridSize = 50,
      centers = 2,
      maxOrigin = 1.0,
      maxDestination = 1.0,
      originRadius = 5.0,
      destinationRadius = 8.0,
      originExponent = 1.5,
      destinationExponent = 1.5,
      costFunction = {d => math.exp(- d / 15.0)}
    ).generateFlows

    // vis is bad - need some filtering + coords adjustement
    //visualization.staticFlowsVisualization(syntheticFlows)

    println(s"Avg synthetic flow = ${syntheticFlows.observedFlows.mean}")

    // try to fit singly constrained
    val model = SinglyConstrainedSpIntModel(syntheticFlows)
    val fittedModel = SinglyConstrainedSpIntModel.fitSinglyConstrainedSpIntModel(model)
    println(s"Fitted parameter = ${fittedModel.fittedParam}")

  }


  /**
    * Dense faster than sparse for flow generation?
    */
  def testSyntheticFlows(): Unit = {
    implicit val rng: Random = new Random
    implicit val mImpl: Matrix.MatrixImplementation = Matrix.Dense(DenseMatrix.DenseBreeze())

    val syntheticFlowsGenerator = PolycentricGridGravityFlowsGenerator(
      gridSize = 50,
      centers = 5,
      maxOrigin = 1.0,
      maxDestination = 1.0,
      originRadius = 5.0,
      destinationRadius = 8.0,
      originExponent = 1.5,
      destinationExponent = 1.5,
      costFunction = {d => math.exp(- d / 15.0)}
    )

    // vis is bad - need some filtering + coords adjustement
    //visualization.staticFlowsVisualization(syntheticFlows)

    val (denseFlows,td) = utils.withTimer[Double,SpatialInteractionModel](_ => syntheticFlowsGenerator.generateFlows)(0.0)

    val sparseGen = syntheticFlowsGenerator.copy()(Matrix.Sparse(SparseMatrix.SparseBreeze()))
    val (sparseFlows,ts) = utils.withTimer[Double,SpatialInteractionModel](_ => sparseGen.generateFlows)(0.0)

    println(s"Avg synthetic dense flow = ${denseFlows.observedFlows.mean} ; t = $td")
    println(s"Avg synthetic sparse flow = ${sparseFlows.observedFlows.mean} ; t = $ts")

  }


}
