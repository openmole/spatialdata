package org.openmole.spatialdata.grid.real

import org.openmole.spatialdata.RasterLayerData
import org.openmole.spatialdata.utils.gis.{GISUtils, LayerSampling}
import org.openmole.spatialdata.utils.io.Shapefile
import org.openmole.spatialdata.utils.osm.api.APIExtractor.{OSMAPIMode, OSMOverpass}

import scala.util.Random

object OSMGridSampling {


  def sampleGridsInLayer(layer: String,nPoints: Int,windowSize: Double,worldWidth: Int,weightAttribute: String = "",mode: OSMAPIMode = OSMOverpass)(implicit rng: Random): Seq[((Double,Double),RasterLayerData[Double])] = {
    //val epsglayer = Shapefile.getLayerEPSG(layer)
    //println(epsglayer)
    //val epsglayer = "ETRS89"//"EPSG:3035"
    val points = LayerSampling.samplePointsInLayer(layer,nPoints,weightAttribute)
    val trPoints = GISUtils.transform(points,Shapefile.getLayerCRS(layer),GISUtils.wgs84)
    trPoints.map{case (lon,lat) => ((lon,lat),OSMGridGenerator(lon,lat,windowSize,worldWidth,mode).generateGrid)}
  }

}
