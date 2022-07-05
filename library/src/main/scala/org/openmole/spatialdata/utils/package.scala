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

  def concat[A](s1: Seq[A], s2: Seq[A]): Seq[A] = s1++s2

  /**
    * tic toc a void function
    * @param fun function
    */
  def withTimer(fun: Unit=> Unit): Unit= {
    val start = System.currentTimeMillis().toDouble
    fun(())
    println("time : "+(System.currentTimeMillis().toDouble-start)+" ms")
  }

  /**
    *
    * @param fun function
    * @tparam A origin type
    * @tparam B destination type
    * @return
    */
  def withTimer[A,B](fun: A => B): A => (B,Double) = {
    a =>
    val start = System.currentTimeMillis().toDouble
    val res = fun(a)
    (res,System.currentTimeMillis().toDouble-start)
  }

  def withTimer[A](a: A): (A,Double) = withTimer[Unit,A](_ => a)(())

  /**
    * tic toc a typed function
    * @param fun function
    * @param a argument
    * @param name name of task
    * @tparam A origin type
    * @tparam B destination type
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
  def log(msg: String): Unit = if(spatialdata.DEBUG||spatialdata.APPLICATION) println(msg)



}
