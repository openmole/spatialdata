package org.openmole.spatialdata.utils

package object math {

  def digits(x: Double, n: Int): Double = scala.math.floor(scala.math.pow(10.0,n)*x)/scala.math.pow(10.0,n)

  def relSquare(x: Double,y: Double): Double = scala.math.pow(2*(x - y)/(x+y) ,2)

}
