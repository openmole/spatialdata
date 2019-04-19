package org.openmole.spatialdata

import org.apache.commons.math3.random.RandomDataGenerator

import scala.util.Random

import collection.JavaConverters._

package object utils {


  implicit class SetDecorator[A](s: Set[A]) {

    // does not use the implicit rng - distinct from the math commons one
    /*def randomTake(n: Int)(implicit rng: Random): Seq[A] = {
      val randomgen = new RandomDataGenerator()
      randomgen.nextSample(s.toSeq.asJavaCollection,n).toSeq.map{_.asInstanceOf[A]}
    }
    def shuffle(implicit rng: Random) = randomTake(s.size)
    */

    def shuffle(implicit rng: Random): Seq[A] = rng.shuffle(s.toSeq)

    def randomTake(n: Int)(implicit rng: Random): Seq[A] = shuffle.take(n)

  }

  def time(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis()
    fun()
    println("time : "+(System.currentTimeMillis()-start)+" ms")
  }


}
