
package spatialdata.measures

import org.dianahep.histogrammar._

object Statistics {


  /**
    * Unbiased moment at a given order
    * @param x
    * @param order
    * @return
    */
  def moment(x: Array[Double],order: Int = 1): Double = x.map{math.pow(_,order.toDouble)}.sum / x.length


  /**
    * Histogram
    * @param x
    * @param breaks
    * @return
    */
  def histogram(x: Array[Double],breaks: Int): Array[(Double,Double)] = {
    val hist = Bin(breaks,x.min,x.max,{d: Double=>d})
    for (d <- x) hist.fill(d)
    val xstep = (x.max - x.min) / breaks
    Array.tabulate(breaks){case i => xstep / 2 + i*xstep}.zip(hist.values.map{_.asInstanceOf[Counting].entries})
  }







}

