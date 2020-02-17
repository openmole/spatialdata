
package org.openmole.spatialdata

import org.openmole.spatialdata.utils.math.Matrix
import org.openmole.spatialdata.vector.SpatialField


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

    implicit class RasterLayerDataDecorator(r: RasterLayerData[Double]){
      // decorate only RasterLayerData[Double] because does not compile with generic type (missing class tag)

      /**
        * Convert centers of raster cells to points with associated cell value
        * @return
        */
      def asSpatialField: SpatialField[Double] = r.zipWithIndex.map{case (row,i) => row.zipWithIndex.map{case (v,j) => ((i+0.5,j+0.5),Array(v))}}.flatten.toMap

      /**
        * Euclidian distance matrix between centers of the raster cells - indexing
        * @return
        */
      /*def distanceMatrix: Matrix = {
        // FIXME not needed, use conversion to spatial field and SpatStat.euclidianDistanceMatrix
      }*/
    }


  }



}