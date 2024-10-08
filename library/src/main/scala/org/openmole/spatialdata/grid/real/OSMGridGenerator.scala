
package org.openmole.spatialdata.grid.real

import org.locationtech.jts.geom._
import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.grid.GridGenerator
import org.openmole.spatialdata.utils.gis.GISUtils
import org.openmole.spatialdata.utils.osm.APIExtractor
import org.openmole.spatialdata.utils.osm.APIExtractor.{OSMAPIMode, OSMOverpass}

import scala.util.{Random, Try}

case class OSMGridGenerator(
                           lon: Double,
                           lat: Double,
                           windowSize: Double,
                           worldWidth: Int,
                           mode: OSMAPIMode = OSMOverpass
                           ) extends GridGenerator {
  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = OSMGridGenerator.OSMBuildingsGrid(lon,lat,windowSize,worldWidth,mode)
}

object OSMGridGenerator {

  implicit val doubleOrdering: Ordering[Double] = Ordering.Double.TotalOrdering

  /**
    * Generate a grid by rasterizing the geometry
    * @param lon lon
    * @param lat lat
    * @param windowSize window size in meters
    * @param worldWidth generated world width
    * @return
    */
  def OSMBuildingsGrid(lon: Double, lat: Double,windowSize: Double, worldWidth: Int,mode: OSMAPIMode): RasterLayerData[Double] = {
    // get polygons
    val (west,south,east,north)=GISUtils.wgs84window(lon,lat,windowSize)
    val g: Geometry = APIExtractor.Buildings.getNegativeBuildingIntersection(south, west, north, east,mode)
    // rescale [0,worldwidth]x[0,worldwidth] to [xmin,xmax]x[ymin,ymax]
    val (xcoords, ycoords) = (g.getCoordinates.map {_.x}, g.getCoordinates.map {_.y})
    val (xmin, xmax) = (xcoords.min, xcoords.max)
    val (ymin, ymax) = (ycoords.min, ycoords.max)
    val (xstep, ystep) = ((xmax - xmin) / (worldWidth+1), (ymax - ymin) / (worldWidth+1))
    val cells: Seq[((Int,Int),Double)] = for {
      (x,i) <- (BigDecimal(xmin) to (xmax - xstep) by xstep).zipWithIndex
      (y,j) <- (BigDecimal(ymin) to (ymax - ystep) by ystep).zipWithIndex
    } yield {
      // note : g is Negative of buildings -> inversed !
      val value = Try(if(
        new GeometryFactory(g.getPrecisionModel,g.getSRID).createPolygon(
          Array(new Coordinate(x.toDouble,y.toDouble),
                new Coordinate((x+xstep).toDouble,y.toDouble),
                new Coordinate((x+xstep).toDouble,(y+ystep).toDouble),
                new Coordinate(x.toDouble,(y+ystep).toDouble),
                new Coordinate(x.toDouble,y.toDouble))).coveredBy(g)) 0.0 else 1.0)
      if(value.isSuccess) ((i,j),value.get) else ((i,j),0.0)
    }
    val values = cells.toMap
    Array.tabulate(worldWidth,worldWidth){case k: (Int,Int)=> values(k)}
  }

}
