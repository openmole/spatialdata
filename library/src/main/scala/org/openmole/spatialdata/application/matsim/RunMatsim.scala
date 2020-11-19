package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}
import org.openmole.spatialdata.utils.io.{GeoPackage, Shapefile}
import org.openmole.spatialdata.vector.{Attributes, Lines, Polygons}
import org.locationtech.jts.geom
import org.locationtech.jts.io.WKTWriter
import org.openmole.spatialdata.utils
import org.openmole.spatialdata.utils.gis.GeometryUtils

import scala.util.Random

object RunMatsim extends App {

  if (args.length>0) {
    args(0) match {
      case "--network" => Network.runNetworkProcessing(args)
      case "--synthpop" => Population.runPopulationConstruction(args)
      case _ => ()
    }
  }

}
