package spatialdata.network

import scala.util.Random


/**
  * A trait to produce networks
  */
trait NetworkGenerator {

  /**
    * Generate a network
    * @param rng
    * @return
    */
  def generateNetwork(implicit rng: Random): Network

}
