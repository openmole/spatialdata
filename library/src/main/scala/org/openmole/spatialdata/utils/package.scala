package org.openmole.spatialdata

package object utils {


  def time(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis()
    fun()
    println("time : "+(System.currentTimeMillis()-start)+" ms")
  }


}
