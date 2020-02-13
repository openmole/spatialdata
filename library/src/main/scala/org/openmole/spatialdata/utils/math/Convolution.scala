package org.openmole.spatialdata.utils.math

import org.apache.commons.math3.complex.Complex
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import org.apache.commons.math3.util.MathArrays

import scala.math.{ceil, log, pow}
import scala.reflect.ClassTag

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


  def padLeftRight[T: ClassTag](x: Array[T],v: T, left: Int, right: Int): Array[T] = {
    //x.padTo(x.length + left, v).reverse.padTo(x.length + left + right, v).reverse
    // actually this is cleaner
    Array.fill(left)(v) ++ x ++ Array.fill(right)(v)
  }

  def cropLeftRight[T: ClassTag](x: Array[T], left: Int, right: Int): Array[T] = {
    x.drop(left).dropRight(right)
  }

  def pad2D(x: Array[Array[Double]], v: Double, topAddRows: Int, bottomAddRows: Int, leftAddCols: Int, rightAddCols: Int): Array[Array[Double]] = {
    val emptyRow = Array.fill(x(0).length + leftAddCols + rightAddCols)(v)
    padLeftRight(x.map(padLeftRight(_,v,leftAddCols,rightAddCols)),emptyRow,topAddRows,bottomAddRows)
  }

  def crop2D(x: Array[Array[Double]], topRows: Int, bottomRows: Int, leftCols: Int, rightCols: Int): Array[Array[Double]] = {
    cropLeftRight(x.map(cropLeftRight(_,leftCols,rightCols)),topRows,bottomRows)
  }

  /**
    *  2D convolution
    *  Using bijection [|1,N|]2 ~ [|1,N|] by flattening, after having good paddling
    *
    * @param x
    * @param k
    */
  def convolution2D(x: Array[Array[Double]], k: Array[Array[Double]]): Array[Array[Double]] = {
    assert(k.length%2==1&k(0).length%2==1,"Kernel must be centered")
    //val xpad = x.map { row =>
    //  row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse
    //}.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 })
    val (top,bottom,left,right) = ((k.length - 1)/2,(k.length - 1)/2 + (x.length+1)%2 ,(k(0).length - 1)/2,(k(0).length - 1)/2 + (x(0).length+1)%2)
    val xpad = pad2D(x,0.0,top,bottom,left,right)
    val xpos = Array.fill(x.length, x(0).length) { 1.0 }.map {
      row => row.padTo(2 * (row.length / 2) + 1 + k(0).length, 0.0).reverse.padTo(2 * (row.length / 2) + 1 + 2 * k(0).length, 0.0).reverse
    }.padTo(2 * (x.length / 2) + 1 + k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).reverse.padTo(2 * (x.length / 2) + 1 + 2 * k.length, Array.fill(2 * (x(0).length / 2) + 1 + 2 * k(0).length) { 0.0 }).flatten
    //val kpad = k.map { row => row.padTo(row.length + (xpad(0).length - row.length) / 2, 0.0).reverse.padTo(xpad(0).length, 0.0).reverse }.padTo(k.length + (xpad.length - k.length) / 2, Array.fill(xpad(0).length) { 0.0 }).reverse.padTo(xpad.length, Array.fill(xpad(0).length) { 0.0 })
    val kpadflat = pad2D(k,0.0,0,0,(xpad(0).length - k(0).length)/2, (xpad(0).length - k(0).length)/2).flatten // xpad always of uneven size
    val flatconv = convolution(xpad.flatten, kpadflat)
    //flatconv.zipWithIndex.filter { case (_, j) => xpos(j) > 0 }.map { case (d, _) => d }.sliding(x(0).length, x.length).toArray.reverse
    crop2D(flatconv.grouped(xpad(0).length).toArray,top,bottom,left,right)
  }




  /**
    * convolution 2d with mask using fft (pb: operator is sum by default)
    *  - binary convol here operator is not applied during convol itself but linked to mask
    * @param matrix
    * @param mask
    * @param operator
    */
  // FIXME DOES NOT WORK
  /*def convolution(matrix: Array[Array[Double]],
                  mask: Array[Array[Double]],
                  filter: Double=>Double = {case d => if(d > 0.0)1.0 else 0.0}
                 ): Array[Array[Double]] ={
    // mask must be padded
    val paddedMask: Array[Array[Double]] = Array.fill((matrix.length-mask.length)/2){Array.fill(matrix(0).length){0.0}}++
      mask.map{case row=>Array.fill((matrix(0).length-mask(0).length)/2){0.0}++row++Array.fill((matrix(0).length-mask(0).length)/2){0.0}}++
      Array.fill((matrix.length-mask.length)/2){Array.fill(matrix(0).length){0.0}}
    val convol: Array[Array[Double]] = Convolution.convolution2D(matrix,paddedMask)
    convol.map{
      _.map{
        case d => filter(d)
      }
    }
  }
  */

  /**
    * Naive two dimensional convolution for morpho math - default operator is average (dilation) - replace by product for erosion
    *   (not efficient at all but no math commons to work in the gui)
    * @param matrix
    * @param mask should be of uneven size
    * @param operator sum by default
    * @return
    */
  def convolution2dDirect(matrix: Array[Array[Double]],mask: Array[Array[Double]],
                        //operator: Array[Double]=>Double = {case a => if(a.filter(_>0.0).size>0)1.0 else 0.0})
                        filter: Double=>Double = {case d => if(d > 0.0)1.0 else 0.0}
                       )
  : Array[Array[Double]] = {
    assert(mask.length%2==1&&mask(0).length%2==1,"mask should be of uneven size")
    val sizes = matrix.map(_.length);assert(sizes.max==sizes.min,"array should be rectangular")
    val masksizes = mask.map(_.length);assert(masksizes.max==masksizes.min,"mask should be rectangular")
    val (paddingx,paddingy) = ((mask.length-1)/2,(mask(0).length-1)/2)
    val padded = Array.tabulate(matrix.length+2*paddingx,matrix(0).length+2*paddingy){
      case (i,j) if i<paddingx||i>=(matrix.length+paddingx)||j<paddingy||j>=(matrix(0).length+paddingy) => 0.0
      case (i,j) => matrix(i-paddingx)(j-paddingy)
    }
    val res = Array.fill(matrix.length+2*paddingx,matrix(0).length+2*paddingy)(0.0)
    for(i <- paddingx until (res.length - paddingx);j <- paddingy until (res(0).length-paddingy)){
      val masked = Array.fill(mask.size,mask(0).size)(0.0)
      for(k <- - paddingx to paddingx;l <- - paddingy to paddingy){
        //assert(i+k<matrix.length&j+l<matrix(0).length,"size : "+i+" "+j+" "+k+" "+" "+l+" for a matrix of size "+matrix.length+";"+matrix(0).length)
        masked(k+paddingx)(l+paddingy)=padded(i+k)(j+l)*mask(k+paddingx)(l+paddingy)
      }
      res(i)(j) = filter(masked.flatten.sum)
    }
    //res.zip(matrix).map{case (row,initrow) => row.take(initrow.length + paddingy).takeRight(initrow.length)}.take(matrix.length+paddingx).takeRight(matrix.length)
    res.map{case row => row.slice(paddingy,row.length-paddingy)}.slice(paddingx,res.length-paddingx)
  }

}
