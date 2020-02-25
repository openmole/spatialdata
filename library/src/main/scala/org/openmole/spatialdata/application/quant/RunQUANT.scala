package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.model.spatialinteraction.SinglyConstrainedSpIntModel
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, SparseMatrix}

object RunQUANT extends App {


  args.length match {
    case 0 => println("Usage: --onemode | --multimode")
    case 1 => {
      args(0) match {
        case "--onemode" => calibrateQuantOneMode
        case "--multimode" => calibrateQuantMultiMode
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
    println(s"average predicted flow = ${modelfitted.predictedFlows.mean} ; fitted param = ${modelfitted.fittedParams}")
  }

  /**
    * Multi mode
    */
  def calibrateQuantMultiMode: Unit = {
    println("Running QUANT model with multiple modes")
    utils.log("\tLoading flows data")
    val flowspaths = Array("TObs_1.csv","TObs_2.csv","TObs_3.csv").map{s => s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$s"}
    utils.log("\tLoading distances data")
    val dmatpaths = Array("dis_roads_min.csv","dis_gbrail_min.csv","dis_bus_min.csv").map{s => s"${System.getenv("CS_HOME")}/UrbanDynamics/Data/QUANT/converted/$s"}
    val modelfitted = QUANTMultiMode.quantMultiMode(flowspaths,dmatpaths).fitted
    println(s"fitted params = ${modelfitted.fittedParams}")
  }


}
