package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.{DenseMatrix, Matrix, SparseMatrix}

object RunQUANT extends App {

  implicit val sparseMatrixImplementation: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()

  args.length match {
    case 0 => println("Usage: --onemode | --multimode | --convert-data")
    case 1 => {
      args(0) match {
        case "--onemode" => calibrateQuantOneMode
        case "--multimode" => calibrateQuantMultiMode
        case "--convert-data" => convertData(sparseMatrixImplementation)
      }
    }
  }


  /**
    * one mode with converted data
    *
    *  dmat reading takes ~9sec! => also sparse it
    */
  def calibrateQuantOneMode: Unit = {
    println("Running QUANT model with one mode")
    utils.log("\tLoading flow data")
    val flowspath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/TObs_1.csv"
    utils.log("\tLoading distance data")
    val dmatpath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/dis_roads_min.csv"
    val modelfitted = QUANTOneMode.quantOneMode(flowspath,dmatpath).fitted
    println(s"average predicted flow = ${modelfitted.predictedFlows.mean} ; fitted param = ${modelfitted.fittedParams.toSeq}")
  }

  /**
    * Multi mode
    */
  def calibrateQuantMultiMode: Unit = {
    println("Running QUANT model with multiple modes")
    utils.log("\tLoading flows data")
    val flowspaths =
      //Array("TObs_1.csv","TObs_2.csv","TObs_3.csv").
      Array("TObs_1.bin","TObs_2.bin","TObs_3.bin").
        map{s => s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$s"}
    utils.log("\tLoading distances data")
    val dmatpaths =
      //Array("dis_roads_min.csv","dis_gbrail_min.csv","dis_bus_min.csv").
      Array("dis_roads_min.bin","dis_gbrail_min.bin","dis_bus_min.bin").
      map{s => s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$s"}
    val modelfitted = QUANTMultiMode.quantMultiMode(flowspaths,dmatpaths).fitted
    println(s"fitted params = ${modelfitted.fittedParams.toSeq}")
  }


  /**
    * Read csv and export as serialized bin (for perf reasons)
    */
  def convertData(spMatImpl: SparseMatrix.SparseMatrixImplementation): Unit = {
    // FIXME do not hardcode file names
    val dmatPrefixes = Array("dis_roads_min","dis_gbrail_min","dis_bus_min")
    val flowmatPrefixes = Array("TObs_1","TObs_2","TObs_3")

    // FIXME test different dmat sparsity param values + sparsity on actual flows ?
    // exp(-d/d0)>epsilon <=> exp(-d)>epsilon^d0 -> single parameter
    //val sparsityThreshold = math.pow(0.3,60.0)
    val sparsityThreshold = math.pow(0.5,60.0)

    dmatPrefixes.foreach { dmatPrefix =>
      utils.log(s"Converting $dmatPrefix")
      val path = s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$dmatPrefix"
      val dmat = CSV.readSparseMatFromDense(path+".csv", { d => math.exp(-d) > sparsityThreshold })
      Binary.writeBinary(dmat, path+".bin")
    }

    flowmatPrefixes.foreach{ flowMatPrefix =>
      utils.log(s"Converting $flowMatPrefix")
      val path = s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$flowMatPrefix"
      val flowmat = CSV.readSparseMat(path+".csv")
      Binary.writeBinary(flowmat,path+".bin")
    }

  }



}
