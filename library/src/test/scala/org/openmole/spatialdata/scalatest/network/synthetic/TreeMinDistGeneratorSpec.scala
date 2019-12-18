package org.openmole.spatialdata.scalatest.network.synthetic

import org.openmole.spatialdata.network.synthetic.TreeMinDistGenerator
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class TreeMinDistGeneratorSpec extends AnyFlatSpec {

  implicit val rng = new Random

  "A TreeMinDist network" should "be planar" in {
    (0 to 10).foreach { _ =>
      assert(TreeMinDistGenerator(10+rng.nextInt(50)).generateNetwork.isPlanar)
    }
  }

  "A TreeMinDist network" should "not have cycles" in {

  }

}
