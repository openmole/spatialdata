

package object spatialdata {

  val DEBUG = true


  /**
    * RasterLayerData are two dimensional arrays of Numeric values
    * TODO keep the name RasterLayer for a wrapper with more properties
    */
  type RasterLayerData[N] = Array[Array[N]]


  /**
    * RasterData sequence of layer data
    */
  type RasterData[N] = Seq[RasterLayerData[N]]

  type RasterDim = Either[Int,(Int,Int)]

  implicit def rasterDimConversion(i:Int): RasterDim = Left(i)
  implicit def rasterDimConversion(c:(Int,Int)): RasterDim = Right(c)


  /**
    * Point in 2D
    */
  type Point2D = (Double,Double)

  /**
    * Geographical coordinate (lon,lat)
    */
  type Coordinate = (Double,Double)


}
