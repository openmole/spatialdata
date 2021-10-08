package org.openmole.spatialdata.application.networkgrowth

import org.openmole.spatialdata.network.real.OSMNetworkGenerator
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.{GISUtils, RasterUtils}
import org.openmole.spatialdata.utils.io.{GeoPackage, Raster}
import org.openmole.spatialdata.utils.visualization

import scala.util.Random

object RealData {

  /**
    * ! raster and fua must be with the same projection; OSM is WGS84: reproject envelope? - or use wgs84? - issue CRS
    */
  val fuaPath: String = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0/GHS_FUA_UCDB2015_GLOBE_R2019A_54009_1K_V1_0.gpkg"
  val poprasterPath: String = System.getenv("CS_HOME")+"/Data/JRC_EC/GHS/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0/GHS_POP_GPW42015_GLOBE_R2015A_54009_1k_v1_0.tif"

  /**
    * not good to have it hardcoded
    */
  val dataCRS: String = "EPSG:54009"

  def computeRealIndicators(fuanames: Seq[String]): Unit = {
    val fualayername = fuaPath.split("/").last.split("\\.")(0)
    val allfuas = GeoPackage.readGeometry(fuaPath,featureName = fualayername, attributes = Array("eFUA_name","Cntry_ISO"))

    val popraster = Raster.readGeotiff(poprasterPath)

    allfuas.filter(a => fuanames.contains(a._2.getOrElse("eFUA_name","").toString)).foreach{
      case (geom, attrs) =>
        utils.log(s"Processing FUA ${attrs.getOrElse("eFUA_name","")}; envelope: ${geom.getEnvelope.getCoordinates.toSeq}")
        val pop = RasterUtils.cropRaster(popraster, geom.getEnvelope)
        utils.log(s"Total pop: ${RasterUtils.gridCoverage2DAsRawData(pop).flatten.sum}")

        // reproject envelope for OSM bbox
        val env = GISUtils.transform(geom.getEnvelope, dataCRS,"EPSG:4326")
        // rng is not used: dummy here
        val roadnetwork = OSMNetworkGenerator(bbox = (env.getEnvelopeInternal.getMinY, env.getEnvelopeInternal.getMinX, env.getEnvelopeInternal.getMaxY, env.getEnvelopeInternal.getMaxX)).generateNetwork(new Random)
        utils.log(s"OSM road network: ${roadnetwork.links.size} segments")

        // raster to points
        val popPoints = RasterUtils.gridCoverage2DAsPoints(pop)

    }

  }


}
