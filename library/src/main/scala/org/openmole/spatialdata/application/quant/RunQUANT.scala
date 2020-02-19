package org.openmole.spatialdata.application.quant

import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.io.CSV
import org.openmole.spatialdata.utils.math.{DenseMatrix, SparseMatrix}

object RunQUANT extends App {

  calibrateQuantOneMode


  /**
    * one mode with converted data
    *
    *  dmat reading takes ~9sec! => also sparse it
    */
  def calibrateQuantOneMode: Unit = {
    val flowspath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/TObs_1.csv"
    val dmatpath = System.getenv("CS_HOME")+"/UrbanDynamics/Data/QUANT/converted/dis_roads_min.csv"
    //val (flowmat,tflow) = utils.withTimer[String,SparseMatrix](s => CSV.readSparseMat(s))(flowspath)
    //val (dmat,tdmat) = utils.withTimer[String,DenseMatrix](s=>DenseMatrix(CSV.readMat(s)))(dmatpath)
    //println(s"flows mat: ${flowmat.nrows}x${flowmat.ncols}; nentries = ${flowmat.nentries} ; in ${tflow}ms")
    //println(s"dmat: ${dmat.nrows}x${dmat.nrows} ; in ${tdmat}ms")
    val modelfitted = QUANTOneMode(flowspath,dmatpath).fitted
    println(modelfitted.predictedFlows.mean)
  }


}
