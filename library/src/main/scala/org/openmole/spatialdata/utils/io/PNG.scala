package org.openmole.spatialdata.utils.io

import better.files.File
import org.openmole.spatialdata.grid.RasterLayerData
import javax.imageio.ImageIO
import java.io.IOException
import java.awt.image.BufferedImage

object PNG {
  def write(input: RasterLayerData[Double], file: File): Unit = {
    val data = input.map(_.map(v=> {
      if (v == 1.0) 0 else 255 // buildings in black
    }))
    val image = new BufferedImage(data.size, data.head.size, BufferedImage.TYPE_BYTE_GRAY)
    val raster = image.getRaster
    val values = for {
      zi <- data.zipWithIndex
      zj <- data(zi._2).zipWithIndex
    } yield (zi._2, zj._2, zj._1)
    values.foreach{case (i,j,v) => raster.setPixel(i,j,Array(v))}
    try
      ImageIO.write(image, "png", file.toJava)
    catch {
      case e: IOException =>
        throw new RuntimeException(e)
    }
  }
}
