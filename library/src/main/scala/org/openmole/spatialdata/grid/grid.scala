
package org.openmole.spatialdata


package object grid {


  /**
    * RasterLayerData are two dimensional arrays of Numeric values
    * TODO keep the name RasterLayer for a wrapper with more properties
    * FIXME mutable ! -> switch to Vector[Vector]
    */
  type RasterLayerData[N] = Array[Array[N]]

  /*
  implicit class RasterLayerDataDecorator[T,U](d: RasterLayerData[T]) {
    def mapElementWise(f: T => U): RasterLayerData[U] = d.map{row: Array[T] => row.map(f).toArray[U]}
  }*/

  // this breaks everything
  //implicit def rasterLayerDataIsVector[N](r: RasterLayerData[N]): Vector[Vector[N]] = r.map{_.toVector}.toVector


  /**
    * RasterData sequence of layer data
    */
  type RasterData[N] = Seq[RasterLayerData[N]]

  type RasterDim = Either[Int,(Int,Int)]




  val empty = Array(Array(0.0))

  /**
    * String representation of a grid
    * @param world
    * @return
    */
  def gridToString(world: RasterLayerData[Double]): String = {
    world.map{_.map(_ match {case x if x > 0.0 => "+"; case x if x == 0 => "0"; case _ => "0"}).mkString("")}.mkString("\n")
  }


  object Implicits {

    implicit def rasterDimConversion(i:Int): RasterDim = Left(i)
    implicit def rasterDimConversion(c:(Int,Int)): RasterDim = Right(c)

  }



}