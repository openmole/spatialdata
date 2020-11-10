package org.openmole.spatialdata.vector

import org.locationtech.jts.geom
import org.locationtech.jts.geom.GeometryFactory
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.GISUtils

/**
  * A spatial lines data frame
  *  (avoids using userData of Geometry to store attributes, better have a proper data structure)
  *
  * @param lines ordered lines
  * @param attributes map of attributes (line index, attribute name) -> attribute value
  *
  */
case class Lines(
                lines: Seq[geom.LineString],
                attributes: Seq[Attributes]
                ) extends VectorFeatures {

  def get(i: Int): (geom.LineString,Attributes) = (lines(i),attributes(i))

  /**
    * tail: handle case with no attributes
    * @return
    */
  def tail: Lines = attributes.size match {
    case 0 => Lines(lines.tail,attributes)
    case _ => Lines(lines.tail,attributes.tail)
  }

  /**
    *  filter ! does not work
    * @param f filter
    * @return
    */
  def filter(f: ((geom.LineString,Attributes)) => Boolean): Lines = {
    val keep: Seq[Boolean] = lines.map{l => f(l,Map.empty)}.zip(attributes.map{a => f((new GeometryFactory).createLineString(),a)}).map{case (b1,b2) => b1&&b2}
    Lines(lines.zip(keep).filter(_._2).map(_._1),attributes.zip(keep).filter(_._2).map(_._1))
  }

  def transform(source: String, target: String): Lines = {
    val transformed = lines.map(GISUtils.transform(_, source, target).asInstanceOf[geom.LineString])
    //utils.log("Source: "+lines.take(2).map(_.toString).mkString("\n"))
    //utils.log("Target: "+transformed.take(2).map(_.toString).mkString("\n"))
    Lines(
      transformed,
      attributes // should replace transfo attribute if exists? not at the feature level!
    )
  }

}

object Lines {
  val empty = Lines(Seq.empty[geom.LineString], Seq.empty)

  def ++(l1: Lines, l2: Lines): Lines = Lines(l1.lines++l2.lines,l1.attributes++l2.attributes)
}

