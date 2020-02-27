package org.openmole.spatialdata.vector.synthetic

import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
import org.openmole.spatialdata.vector.{Polygons, PolygonsGenerator}

import scala.util.Random

case class GridPolygonsGenerator(
                                xmin: Double,
                                xmax: Double,
                                ymin: Double,
                                ymax: Double,
                                xres: Double,
                                yres: Double
                                ) extends PolygonsGenerator {

  override def generatePolygons(implicit rng: Random): Polygons = GridPolygonsGenerator.gridPolygons(xmin,xmax,ymin,ymax,xres,yres)

}


object GridPolygonsGenerator {


  /**
    * coords between 0 and 1 and given number of steps
    * @param n number of steps
    * @return polygons
    */
  def apply(n: Int): GridPolygonsGenerator = GridPolygonsGenerator(0.0,1.0,0.0,1.0,1.0/n.toDouble,1.0/n.toDouble)


  /**
    * Rq: will skip the last ones to not overflow xmax
    * @param xmin xmin
    * @param xmax xmax
    * @param ymin ymin
    * @param ymax ymax
    * @param xres x resolution
    * @param yres y resolution
    * @return
    */
  def gridPolygons(xmin: Double, xmax: Double, ymin: Double, ymax: Double, xres: Double, yres: Double): Polygons = {
    val (nx,ny) = (math.floor((xmax - xmin) / xres).toInt,math.floor((ymax - ymin) / yres).toInt)
    val geomFactory = new GeometryFactory
    Polygons(
      for {
        i <- 0 until nx
        j <- 0 until ny
        x0 = xmin + i*xres
        y0 = ymin + j*yres
        p = geomFactory.createPolygon(Array(new Coordinate(x0,y0),new Coordinate(x0+xres,y0),new Coordinate(x0+xres,y0+yres),new Coordinate(x0,y0+yres),new Coordinate(x0,y0)))
      } yield p
    )
  }

}

