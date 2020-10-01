package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.network.real.GISFileNetworkGenerator
import org.openmole.spatialdata.utils.io.Gexf

import scala.util.Random

object RunMatsim extends App {
  implicit val rng: Random = new Random
  if(args.length==0) throw new IllegalArgumentException("Must provide data file")
  println("Data file: "+args(0))
  val nw = GISFileNetworkGenerator(args(0)).generateNetwork
  println(nw)
  // export network to matsim format
  Gexf.writeGexf(nw,"/data/outputs/network.gexf")
}
