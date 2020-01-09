package org.openmole.spatialdata.utils

package object math {

  def digits(x: Double, n: Int): Double = scala.math.floor(scala.math.pow(10.0,n)*x)/scala.math.pow(10.0,n)

  def relSquare(x: Double,y: Double): Double = scala.math.pow(2*(x - y)/(x+y) ,2)

  object Implicits {

    implicit class IntCoordinateDecorator(p: (Int,Int)){
      /**
        * Lexicographic order
        * @param p2
        * @return
        */
      def <=(p2: (Int,Int)): Boolean = if(p._1 == p2._1) {p._2 <= p2._2} else p._1 < p2._1
    }

  }


}
