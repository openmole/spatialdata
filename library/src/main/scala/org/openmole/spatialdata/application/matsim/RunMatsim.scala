package org.openmole.spatialdata.application.matsim

import org.openmole.spatialdata.network.real.{GISFileNetworkGenerator, MatsimNetworkGenerator}

import scala.util.Random

object RunMatsim extends App {
  implicit val rng: Random = new Random
  if(args.length<2) throw new IllegalArgumentException("Must provide data file")
  println("Data file: "+args(1))
  args(0) match {
    case "--network" =>
      val nw = GISFileNetworkGenerator(args(1)).generateNetwork
      //println(nw)
      // export network to matsim format
      //Gexf.writeGexf(nw,"/data/outputs/network.gexf")
      MatsimNetworkGenerator.writeMatsimXML(nw, args(2))

    case "--synthpop" =>
      // convert spenser synth pop files to Matsim population

  }


}
