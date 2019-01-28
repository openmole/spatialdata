
package spatialdata.utils.gis

import com.vividsolutions.jts.geom.{Coordinate, GeometryFactory}
import org.geotools.geometry.jts.JTS
import org.geotools.referencing.CRS
import org.opengis.referencing.crs.CoordinateReferenceSystem

object GISUtils {

  val wgs84 = CRS.parseWKT("GEOGCS[\"WGS 84\",DATUM[\"WGS_1984\",SPHEROID[\"WGS 84\",6378137,298.257223563,AUTHORITY[\"EPSG\",\"7030\"]],AUTHORITY[\"EPSG\",\"6326\"]],PRIMEM[\"Greenwich\",0,AUTHORITY[\"EPSG\",\"8901\"]],UNIT[\"degree\",0.01745329251994328,AUTHORITY[\"EPSG\",\"9122\"]],AUTHORITY[\"EPSG\",\"4326\"]]")


  /**
    * Transform a set of points
    * @param points
    * @param sourceCRS
    * @param targetCRS
    * @return
    */
  def transform(points: Seq[spatialdata.Coordinate],sourceCRS: String,targetCRS: String): Seq[spatialdata.Coordinate] = {
    val source = CRS.decode(sourceCRS)
    val target = CRS.decode(targetCRS)
    val transform = CRS.findMathTransform(source, target, true)
    val geomFactory = new GeometryFactory
    points.map { p => {
      val c = JTS.transform(geomFactory.createPoint(new Coordinate(p._1, p._2)), transform).getCoordinate;
      (c.x, c.y)
      }
    }
  }

  def transform(points: Seq[spatialdata.Coordinate],source: CoordinateReferenceSystem,target: CoordinateReferenceSystem): Seq[spatialdata.Coordinate] = {

    val transform = CRS.findMathTransform(source, target, true)
    val geomFactory = new GeometryFactory
    points.map { p => {
      val c = JTS.transform(geomFactory.createPoint(new Coordinate(p._1, p._2)), transform).getCoordinate;
      (c.x, c.y)
    }
    }
  }


}

