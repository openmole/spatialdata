package org.openmole.spatialdata.vector

import org.locationtech.jts.geom

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

  def tail: Lines = Lines(lines.tail,attributes.tail)

}

object Lines {
  val empty = Lines(Seq.empty[geom.LineString], Seq.empty)

  //def ++(l1: Lines, l2: Lines): Lines = Lines(l1.lines++l2.lines, l1.attributes++l2.attributes.map{case ((i,s),v) => ((i+l1.lines.size,s),v)})
  def ++(l1: Lines, l2: Lines): Lines = Lines(l1.lines++l2.lines,Seq.empty)
}

