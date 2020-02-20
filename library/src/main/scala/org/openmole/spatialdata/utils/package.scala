package org.openmole.spatialdata

import org.apache.commons.math3.random.RandomDataGenerator
import org.openmole.spatialdata
import org.openmole.spatialdata.utils.math.Stochastic

import scala.util.Random
import collection.JavaConverters._

package object utils {

  object Implicits {

    implicit class IterableDecorator[T](s: Iterable[T]) {

      def sampleWithReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithReplacement(s, samples)

      def sampleWithoutReplacement(samples: Int)(implicit rng: Random): Vector[T] = Stochastic.sampleWithoutReplacement(s, samples)

      def shuffle(implicit rng: Random): Seq[T] = rng.shuffle(s.toSeq)

    }

  }

  /**
    * tic toc a void function
    * @param fun
    */
  def withTimer(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis()
    fun()
    println("time : "+(System.currentTimeMillis()-start)+" ms")
  }

  /**
    *
    * @param fun
    * @param a
    * @tparam A
    * @tparam B
    * @return
    */
  def withTimer[A,B](fun: A => B): A => (B,Double) = {
    a =>
    val start = System.currentTimeMillis()
    val res = fun(a)
    (res,System.currentTimeMillis()-start)
  }

  def withTimer[A](a: A): (A,Double) = withTimer[Unit,A](_ => a)()

  /**
    * tic toc a typed function
    * @param fun
    * @param a
    * @param name
    * @tparam A
    * @tparam B
    * @return
    */
  def timerLog[A,B](fun: A => B,a: A, name: String): B = {
    val (res,t) = withTimer[A,B](fun)(a)
    log(s"$name: ${t}ms")
    res
  }

  def timerLog[A](a: A, name: String): A = timerLog[Unit,A](_ => a, (), name)

  /**
    * log if debug
    * @param msg
    */
  def log(msg: String): Unit = if(spatialdata.DEBUG) println(msg)



}
