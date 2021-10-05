package org.openmole.spatialdata.application.networkgrowth

import org.openmole.spatialdata.utils.io.GeoPackage

object RealData {

  val fuapath: String = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0_WGS84.gpkg"

  def computeRealIndicators(): Unit = {
    val fualayername = fuapath.split("/").last.split("\\.")(0)
    val allfuas = GeoPackage.readGeometry(fuapath,featureName = fualayername, attributes = Array("eFUA_name","Cntry_ISO"))


  }


}
