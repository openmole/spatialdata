package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.{Binary, CSV}
import org.openmole.spatialdata.utils.math.SparseMatrix

object QUANT {

  /**
   * file names (hardcoded should be default only, and options on the command line)
   * @param mode csv or bin
   * @return
   */
  def QUANTObservedFlowsFiles(mode: String): Map[Int, String] = (1 to 3).map(i => (i,QUANTDataPrefix+"TObs_"+i+"."+mode)).toMap

  def QUANTDistanceFiles(mode: String): Map[Int, String] = Seq("roads", "gbrail", "bus").zipWithIndex.map{case (s,i) => (i,QUANTDataPrefix+"dis_"+s+"_min."+mode)}.toMap


  val QUANTDataPrefix: String = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/"

  val MSOAFile: String = System.getenv("CS_HOME")+"/UrbanDynamics/Data/OrdnanceSurvey/MSOA/EnglandWalesScotland_MSOAWGS84.shp"
  val msoaID: String = "MSOA11CD"

  implicit val sparseMatrixImplementation: SparseMatrix.SparseMatrixImplementation = SparseMatrix.SparseBreeze()


  /**
   * one mode with converted data
   *
   *  dmat reading takes ~9sec! => also sparse it
   */
  def calibrateQUANTOneMode(): Unit = {
    utils.log("Running QUANT model with one mode")
    utils.log("    Loading flow data")
    val flowspath = QUANTObservedFlowsFiles("csv")(1)
    utils.log("    Loading distance data")
    val dmatpath = QUANTDistanceFiles("csv")(1)
    val modelfitted = QUANTOneMode.QUANTOneMode(loadQUANTFlows(Array(flowspath)).head,loadQUANTDistances(Array(dmatpath)).head).fitted
    utils.log(s"average predicted flow = ${modelfitted.predictedFlows.mean} ; fitted param = ${modelfitted.fittedParams.toSeq}")
  }

  /**
   * Multi mode
   */
  def calibrateQUANTMultiMode(): Unit = {
    utils.log("Running QUANT model with multiple modes")

    val flowspaths = QUANTObservedFlowsFiles("bin").values.toArray
    utils.log(s"   Loading flows data: ${flowspaths.toSeq}")

    val dmatpaths = QUANTDistanceFiles("bin").values.toArray
    utils.log(s"   Loading distances data: ${dmatpaths.toSeq}")

    val modelfitted = QUANTMultiMode.QUANTMultiMode(loadQUANTFlows(flowspaths),loadQUANTDistances(dmatpaths)).fitted

    utils.log(s"fitted params = ${modelfitted.fittedParams.toSeq}")
  }

  def loadQUANTFlows(files: Array[String], csvInput: Boolean = false): Array[SparseMatrix] = {
    val flowmats = if (csvInput) files.map(sparseFlowsFile => CSV.readSparseMat(sparseFlowsFile))
    else files.map(Binary.readBinary[SparseMatrix])
    utils.log(s"sparse QUANT flowmats: ${flowmats.toSeq.map(m => s"${m.nrows}x${m.ncols}")}")
    flowmats
  }

  def loadQUANTDistances(files: Array[String], csvInput: Boolean = false): Array[SparseMatrix] = {
    val dmats = if (csvInput) files.map(dmatFile =>
      CSV.readSparseMatFromDense(
        dmatFile,
        {d=> math.exp( - d / 60.0) > 0.3},
        {d => math.exp( - d / 60.0)}
      ))
    else files.map(Binary.readBinary[SparseMatrix])
    utils.log(s"sparse QUANT dmats: ${dmats.toSeq.map(m => s"${m.nrows}x${m.ncols}")}")
    dmats
  }


  /**
   * Read csv and export as serialized bin (for perf reasons)
   */
  def convertData()(implicit sparseMatrixImplementation: SparseMatrix.SparseMatrixImplementation): Unit = {
    val dmatPrefixes = Array("dis_roads_min","dis_gbrail_min","dis_bus_min")
    val flowmatPrefixes = Array("TObs_1","TObs_2","TObs_3")

    // exp(-d/d0)>epsilon <=> exp(-d)>epsilon^d0 -> single parameter
    val sparsityThreshold = math.pow(0.3,60.0)
    //val sparsityThreshold = math.pow(0.5,60.0)

    dmatPrefixes.foreach { dmatPrefix =>
      utils.log(s"Converting $dmatPrefix")
      val path = s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$dmatPrefix"
      val dmat = CSV.readSparseMatFromDense(
        path+".csv",
        { d => math.exp(-d) > sparsityThreshold },
        {d => math.exp( - d / 60.0)}
      )
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
