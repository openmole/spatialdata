
package spatialdata.sampling

import org.geotools.referencing.CRS
import spatialdata.RasterLayerData
import spatialdata.osm.OSMGridGenerator
import spatialdata.utils.gis.GISUtils
import spatialdata.utils.io.Shapefile

import scala.util.Random

object OSMGridSampling {


  def sampleGridsInLayer(layer: String,nPoints: Int,windowSize: Double,worldWidth: Int,weightAttribute: String = "")(implicit rng: Random): Seq[((Double,Double),RasterLayerData[Double])] = {
    //val epsglayer = Shapefile.getLayerEPSG(layer)
    //println(epsglayer)
    //val epsglayer = "ETRS89"//"EPSG:3035"
    val points = SpatialSampling.samplePointsInLayer(layer,nPoints,weightAttribute)
    val trPoints = GISUtils.transform(points,Shapefile.getLayerCRS(layer),GISUtils.wgs84)
    trPoints.map{case (lon,lat) => ((lon,lat),OSMGridGenerator(lon,lat,windowSize,worldWidth).generateGrid)}
  }

}



