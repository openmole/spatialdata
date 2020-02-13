package org.openmole.spatialdata

import org.apache.commons.math3.random.RandomDataGenerator
import org.openmole.spatialdata
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random
import collection.JavaConverters._

package object utils {

  object Implicits {

    implicit class TraversableDecorator[T](s: Traversable[T]) {

      def sampleWithReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithReplacement(s, samples)

      def sampleWithoutReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithoutReplacement(s, samples)

      def shuffle(implicit rng: Random): Seq[T] = rng.shuffle(s.toSeq)

    }

  }

  def withTimer(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis()
    fun()
    println("time : "+(System.currentTimeMillis()-start)+" ms")
  }

  def withTimer[A,B](fun: A => B)(a: A): (B,Double) = {
    val start = System.currentTimeMillis()
    val res = fun(a)
    (res,System.currentTimeMillis()-start)
  }

  // FIXME does not work
  def withTimer[A](a: A): (A,Double) = withTimer[Unit,A]{ _ => a}()

  def log(msg: String): Unit = if(spatialdata.DEBUG) println(msg)



}
