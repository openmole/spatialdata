package org.openmole.spatialdata.scalatest.network.synthetic

import org.openmole.spatialdata.network.Network
import org.openmole.spatialdata.network.synthetic.RandomNetworkGenerator
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random


class RandomNetworkGeneratorSpec extends AnyFlatSpec {

  implicit val rng: Random = new Random

  "An empty random network" should "be the empty network" in {
    val emptyRandomNetworkGenerator = new RandomNetworkGenerator(0,0)
    assert(emptyRandomNetworkGenerator.generateNetwork==Network.empty)
  }

  "A network with no nodes but links" should "be empty" in {
    val emptyRandomNetworkGenerator = new RandomNetworkGenerator(0,100)
    assert(emptyRandomNetworkGenerator.generateNetwork==Network.empty)
  }

  "A planar random network" should "be planar" in {
    (0 to 100).foreach{_ =>
      val emptyRandomNetworkGenerator = new RandomNetworkGenerator(rng.nextInt(10),rng.nextInt(10),planarize = true)
      assert(emptyRandomNetworkGenerator.generateNetwork.isPlanar)
    }
  }

  // not useful
  /*"A directed random network" should "be directed" in {

  }*/

}

