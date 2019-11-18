package org.openmole.spatialdata

import org.apache.commons.math3.random.RandomDataGenerator
import org.openmole.spatialdata
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random
import collection.JavaConverters._

package object utils {


  implicit class TraversableDecorator[T](s: Traversable[T]){

    def sampleWithoutReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithoutReplacementBy(s,{_: T => 1/s.size}, samples)

  }


  implicit class SetDecorator[A](s: Set[A]) {

    // does not use the implicit rng - distinct from the math commons one
    /*def randomTake(n: Int)(implicit rng: Random): Seq[A] = {
      val randomgen = new RandomDataGenerator()
      randomgen.nextSample(s.toSeq.asJavaCollection,n).toSeq.map{_.asInstanceOf[A]}
    }
    def shuffle(implicit rng: Random) = randomTake(s.size)
    */

    def shuffle(implicit rng: Random): Seq[A] = rng.shuffle(s.toSeq)

    // FIXME does not work for sampling with replacement !
    def randomTake(n: Int)(implicit rng: Random): Seq[A] = shuffle.take(n)

  }

  def time(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis()
    fun()
    println("time : "+(System.currentTimeMillis()-start)+" ms")
  }

  def withTimer[A,B](fun: A => B)(a: A): (B,Double) = {
    val start = System.currentTimeMillis()
    val res = fun(a)
    (res,System.currentTimeMillis()-start)
  }

  def log(msg: String): Unit = if(spatialdata.DEBUG) println(msg)



}
