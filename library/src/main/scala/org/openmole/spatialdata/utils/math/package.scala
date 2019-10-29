package org.openmole.spatialdata.utils

package object math {

  def digits(x: Double, n: Int): Double = scala.math.floor(scala.math.pow(10.0,n)*x)/scala.math.pow(10.0,n)

}
