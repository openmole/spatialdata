package org.openmole.spatialdata.utils.math

object Time {

  /**
    * Tranform a double decimal hour of day into HH:MM:SS (rq: could use java date)
    * @param time time
    * @return
    */
  def decimalHourToString(time: Double): String = {
    val hours = time.toInt%24
    val minutes = ((time - hours.toDouble) * 60.0).toInt
    val seconds = (((time - hours.toDouble)* 60.0 - minutes.toDouble)*60.0).toInt
    hours.toString+":"+minutes.toString+":"+seconds.toString
  }

}
