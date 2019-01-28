
package spatialdata.osm

import com.vividsolutions.jts.geom._
import com.vividsolutions.jts.geom.impl.CoordinateArraySequenceFactory
import spatialdata.RasterLayerData
import spatialdata.synthetic.grid.GridGenerator

import scala.util.Random

case class OSMGridGenerator(
                           lon: Double,
                           lat: Double,
                           windowSize: Double,
                           worldWidth: Int
                           ) extends GridGenerator {
  override def generateGrid(implicit rng: Random): RasterLayerData[Double] = OSMGridGenerator.OSMBuildingsGrid(lon,lat,windowSize,worldWidth)
}

object OSMGridGenerator {

  /**
    * Generate a grid by rasterizing the geometry
    * @param lon
    * @param lat
    * @param windowSize
    * @param worldWidth
    * @return
    */
  def OSMBuildingsGrid(lon: Double, lat: Double,windowSize: Double, worldWidth: Int): RasterLayerData[Double] = {
    // get polygons
    val (x, y) = BuildingExtractor.WGS84ToPseudoMercator(lon, lat)
    val (west, south) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x - windowSize / 2, y - windowSize / 2)
    val (east, north) = BuildingExtractor.PseudoMercatorToWGS84Mercator(x + windowSize / 2, y + windowSize / 2)
    val g: Geometry = BuildingExtractor.getNegativeBuildingIntersection(south, west, north, east)
    // rescale [0,worldwidth]x[0,worldwidth] to [xmin,xmax]x[ymin,ymax]
    val (xcoords, ycoords) = (g.getCoordinates.map {_.x}, g.getCoordinates.map {_.y})
    val (xmin, xmax) = (xcoords.min, xcoords.max)
    val (ymin, ymax) = (ycoords.min, ycoords.max)
    val (xstep, ystep) = ((xmax - xmin) / (worldWidth+1), (ymax - ymin) / (worldWidth+1))
    val cells: Seq[((Int,Int),Double)] = for {
      (x,i) <- (xmin to (xmax - xstep) by xstep).zipWithIndex
      (y,j) <- (ymin to (ymax - ystep) by ystep).zipWithIndex
    } yield{
      ((i,j), if(new GeometryFactory(g.getPrecisionModel,g.getSRID).createPolygon(Array(new Coordinate(x,y),new Coordinate(x+xstep,y),new Coordinate(x+xstep,y+ystep),new Coordinate(x,y+ystep),new Coordinate(x,y))).coveredBy(g)) 1.0 else 0.0
        )
    }
    val values = cells.toMap
    println(values.keys.size)
    Array.tabulate(worldWidth,worldWidth){case k=> values(k)}
  }

}
