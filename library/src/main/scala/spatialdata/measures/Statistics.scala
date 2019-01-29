
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
  def histogram(x: Array[Double],breaks: Int,filter: Double => Boolean = _ => true,display:Boolean = false): Array[(Double,Double)] = {
    val xx = x.filter(filter)
    val hist = Bin(breaks,xx.min,xx.max,{d: Double=>d})
    for (d <- xx) hist.fill(d)

    if(display) {
      import org.dianahep.histogrammar.ascii._
      println(hist.ascii)
    }

    val xstep = (xx.max - xx.min) / breaks
    Array.tabulate(breaks){case i => xstep / 2 + i*xstep}.zip(hist.values.map{_.asInstanceOf[Counting].entries})
  }

  /**
    * biased estimator of the std
    * @param x
    * @return
    */
  def std(x: Array[Double]): Double = {
    val ex = moment(x)
    math.sqrt(moment(x,2) - ex*ex)
  }






}

