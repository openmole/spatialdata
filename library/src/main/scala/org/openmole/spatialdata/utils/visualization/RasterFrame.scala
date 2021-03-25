package org.openmole.spatialdata.utils.visualization

import java.awt._

import javax.swing.{JComponent, JFrame, WindowConstants}
import javax.swing.plaf.ComponentUI
import org.openmole.spatialdata.grid.RasterLayerData
import org.openmole.spatialdata.utils

case class RasterFrame(
                        raster: RasterLayerData[Double],
                        gradientColors: (Color,Color) = (Color.WHITE, Color.BLACK),
                        projection: Array[Array[Double]] => Array[Array[Double]] = normalization,
                        frameWidth: Int = 600,
                        frameHeight: Int = 600
                      ) extends JFrame() {
  def init(): Unit = {
    utils.log(s"Visualizing raster of size ${raster.length}x${raster(0).length}")
    frameInit()
    setSize(frameWidth,frameHeight)
    setLocation(100,100)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    add(new JComponent {
      setUI(new ComponentUI {
        override def paint(g: Graphics, c: JComponent): Unit = {
          val gg: Graphics2D = g.create.asInstanceOf[Graphics2D]
          gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
          gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND))
          val colorArray = projection(raster).map{_.map{d =>
            new Color(
              (gradientColors._1.getRed*(1 - d) + d*gradientColors._2.getRed).toInt,
              (gradientColors._1.getGreen*(1 - d) + d*gradientColors._2.getGreen).toInt,
              (gradientColors._1.getBlue*(1 - d) + d*gradientColors._2.getBlue).toInt
            )}}
          val (rowstep,colstep) = (frameHeight / colorArray.length,frameWidth/colorArray(0).length)
          colorArray.zipWithIndex.foreach{case (row,i)=>
            row.zipWithIndex.foreach{case (color,j) =>
              gg.setColor(color)
              gg.fillRect(j*colstep,i*rowstep,colstep,rowstep)
            }
          }
        }
      })
      setOpaque(true)
      setForeground(Color.BLACK)
      setBackground(Color.WHITE)
    })
    setVisible(true)
    //Thread.sleep(10000)
  }
}