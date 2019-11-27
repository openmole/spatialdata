package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import org.apache.commons.math3.util.MathArrays

import scala.math.{ceil, log, pow}

object Convolution {

  /**
    * Generic convol for double Arrays (in O(nlog(n)), using FFT)
    *  FIXME implement general x*y and call this kernel convol
    * @param x
    * @param k centered kernel
    * @return y = x*k with y_i = \sum_{j=1}{|K|}{x_{i-j-|K|/2}*k_j}
    */
  def convolution(x: Array[Double], k: Array[Double]): Array[Double] = {
    assert(k.length%2==1,"Kernel must be centered")
    assert(k.length <= x.length,"Kernel must be smaller than vector") // fails it k is too large and not padded to a power of two; anyway no reason to do that as retrieve x part only
    val xl = pow(2.0, ceil(log(x.length) / log(2.0)) + 1)
    val xp = x.padTo(x.length + (xl.toInt - x.length) / 2, 0.0).reverse.padTo(xl.toInt, 0.0).reverse
    val kp = k.padTo(k.length + (xl.toInt - k.length) / 2, 0.0).reverse.padTo(xl.toInt, 0.0).reverse
    val tr = new FastFourierTransformer(DftNormalization.STANDARD)
    val ftx = tr.transform(xp, TransformType.FORWARD)
    val ftk = tr.transform(kp, TransformType.FORWARD)
    val real = MathArrays.ebeSubtract(MathArrays.ebeMultiply(ftx.map { z => z.getReal }, ftk.map { z => z.getReal }), MathArrays.ebeMultiply(ftx.map { z => z.getImaginary }, ftk.map { z => z.getImaginary }))
    val im = MathArrays.ebeAdd(MathArrays.ebeMultiply(ftx.map { z => z.getReal }, ftk.map { z => z.getImaginary }), MathArrays.ebeMultiply(ftx.map { z => z.getImaginary }, ftk.map { z => z.getReal }))
    val trinv = tr.transform(Array.tabulate(real.length) { i => new Complex(real(i), im(i)) }, TransformType.INVERSE).map { z => z.getReal }
    trinv.splitAt(trinv.length - x.length / 2)._2 ++ trinv.splitAt(x.length - x.length / 2)._1
  }

  /**
    * Direct computation of convolution O(n2)
    *
    * @param x
    * @param k centered kernel
    * @return
    */
  def directConvol(x: Array[Double], k: Array[Double]): Array[Double] = {
    assert(k.length%2==1,"Kernel must be centered")
    val xpadded = x.padTo(x.length + (k.length-1)/2, 0.0).reverse.padTo(x.length + k.length - 1, 0.0).reverse
   // Array.tabulate(x.length + k.length) { i => MathArrays.ebeMultiply(k.reverse, xpadded.splitAt(i + 1)._2.splitAt(k.length)._1).sum }
    Array.tabulate(x.length) { i => MathArrays.ebeMultiply(k.reverse, xpadded.splitAt(i)._2.splitAt(k.length)._1).sum }
  }

  /*
  def pad2D(x: Array[Array[Double]], v: Double, topAddRows: Int, bottomAddRows: Int, leftAddCols: Int, rightAddCols: Int): Array[Array[Double]] = {
    x.map { row =>
      row.padTo(leftAddCols, 0.0).reverse.padTo(rightAddCols, 0.0).reverse
    }.padTo(topAddRows, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 })
  }
  */

  /**
    *  2D convolution
    *  Using bijection [|1,N|]2 ~ [|1,N|] by flattening, after having good paddling
    *
    * @param x
    * @param k
    */
  def convolution2D(x: Array[Array[Double]], k: Array[Array[Double]]): Array[Array[Double]] = {
    /*val xpad = x.map { row =>
      row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse
    }.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 })
    val xpos = Array.fill(x.length, x(0).length) { 1.0 }.map {
      row => row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse
    }.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).flatten
    val kpad = k.map { row => row.padTo(row.length + (xpad(0).length - row.length) / 2, 0.0).reverse.padTo(xpad(0).length, 0.0).reverse }.padTo(k.length + (xpad.length - k.length) / 2, Array.fill(xpad(0).length) { 0.0 }).reverse.padTo(xpad.length, Array.fill(xpad(0).length) { 0.0 })
    val flatconv = convolution(xpad.flatten, kpad.flatten)
    flatconv.zipWithIndex.filter { case (_, j) => xpos(j) > 0 }.map { case (d, _) => d }.sliding(x(0).length, x.length).toArray.reverse
    */
    println(x.flatten.length)
    println(k.flatten.length)
    val xn = x.flatten.length
    val kn = k.flatten.length
    convolution(x.flatten.padTo(xn + kn/2,0.0).reverse.padTo(xn + kn,0.0).reverse,k.flatten).drop((kn-xn)/2).dropRight((kn-xn)/2).grouped(x(0).length).toArray
  }

}
