package org.openmole.spatialdata.utils.osm.domain

import java.io.Serializable

/**
  * @author kalle
  * @since 2013-05-01 15:42
  */
@SerialVersionUID(1l)
class Node() extends OsmObject with Serializable {
  def accept[R](visitor: OsmObjectVisitor[R]) = visitor.visit(this)

  def this(id: Long, latitude: Double, longitude: Double, tags: String*) {
    this()
    this.latitude = latitude
    this.longitude = longitude
    setId(id)
    var i = 0
    while ( {
      i < tags.length
    }) {
      setTag(tags(i), tags(i + 1))

      i += 2
    }
  }

  private var latitude = .0
  private var longitude = .0
  private var waysMemberships: java.util.List[Way] = _

  def addWayMembership(way: Way): Unit = {
    if (waysMemberships == null) waysMemberships = new java.util.ArrayList[Way](5)
    else { // don't add membership to the same way twice
      // this happens for instance when this is the start and stop in a polygon.
      import scala.collection.JavaConversions._
      for (wayMembership <- waysMemberships) {
        if (way == wayMembership) return
      }
    }
    waysMemberships.add(way)
  }

  def getWaysMemberships = waysMemberships

  def setWaysMemberships(waysMemberships: java.util.List[Way]) = {
    this.waysMemberships = waysMemberships
  }

  def getLatitude = latitude

  def setLatitude(latitude: Double) = {
    this.latitude = latitude
  }

  def getLongitude = longitude

  def setLongitude(longitude: Double) = {
    this.longitude = longitude
  }

  def getY = getLatitude

  def setY(latitude: Double) = {
    setLatitude(latitude)
  }

  def getX = getLongitude

  def setX(longitude: Double) = {
    setLongitude(longitude)
  }

  override def toString = "Node{" + super.toString + "latitude=" + latitude + ", longitude=" + longitude + ", waysMemberships.size=" + (if (waysMemberships == null) "null"
  else waysMemberships.size) + '}'
}
