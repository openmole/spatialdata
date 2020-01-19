package org.openmole.spatialdata.utils

import org.openmole.spatialdata.grid._
import org.openmole.spatialdata.vector.Point
import org.openmole.spatialdata.network.{Network, Node}
import javax.swing._
import java.awt._
import java.lang
import java.lang.System

import javax.swing.plaf.ComponentUI

package object visualization {

  import Color._
  val colors = Vector(BLACK,BLUE,MAGENTA,ORANGE,PINK,RED,CYAN,YELLOW,DARK_GRAY,GRAY,GREEN,LIGHT_GRAY)//,WHITE)

  def palette(i: Int): Color = {colors(i%colors.size)}

  case class NetworkFrame(networks: Seq[Network],
                          frameWidth: Int = 600,
                          frameHeight: Int = 600,
                          nodeSize: Int = 6,
                          margin: Int = 50,
                          withLabel: Boolean = true,
                          edgeColors: Seq[Int] = Seq.empty ,
                          nodeColorClasses: Node => Int = {_ => 0},
                          nodePositioning: Node => Point = {n => n.position}
                         ) extends JFrame() {
    def init: Unit = {
      frameInit()
      setSize(frameWidth+2*margin,frameHeight+2*margin)
      setLocation(100,100)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      add(new JComponent {
        setUI(new ComponentUI {
          override def paint(g: Graphics, c: JComponent): Unit = {
            val gg: Graphics2D = g.create.asInstanceOf[Graphics2D]
            gg.setColor(Color.BLACK)
            gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
            gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

            val ecolors = if(edgeColors.nonEmpty) edgeColors else Seq.fill(networks.size)(0)
            networks.zip(ecolors).foreach {case (network, ecolor) =>
              network.nodes.foreach { n => {
                val np = nodePositioning(n)
                val (x, y) = (margin + (np._1 * frameWidth - nodeSize / 2).toInt, margin + (np._2 * frameHeight - nodeSize / 2).toInt)
                //if (nodeColorClasses.isDefined) gg.setColor(palette(nodeColorClasses.get(n))) //slight perf loss without option?
                gg.setColor(palette(nodeColorClasses(n)))
                gg.fillRect(x, y, nodeSize, nodeSize)
                if (withLabel) gg.drawString(n.id.toString, x + nodeSize, y + nodeSize)
              }
              }
              gg.setColor(palette(ecolor))
              network.links.foreach { l => gg.drawLine(
                margin + (nodePositioning(l.e1)._1 * frameWidth).toInt,
                margin + (nodePositioning(l.e1)._2 * frameHeight).toInt,
                margin + (nodePositioning(l.e2)._1 * frameWidth).toInt,
                margin + (nodePositioning(l.e2)._2 * frameHeight).toInt) }
            }
          }
        })
        setOpaque(true)
        setForeground(Color.BLACK)
        setBackground(Color.WHITE)
      })
      setVisible(true)
      Thread.sleep(10000)
    }
  }


  /**
    * Quick visu for debugging purposes
    * @param network
    */
  def staticNetworkVisualization(networks: Seq[Network],
                                 withLabel: Boolean = false,
                                 edgeColors: Seq[Int] = Seq.empty,
                                 nodeColorClasses: Node=>Int={_ => 0},
                                 nodePositioning: Node => Point = {n => n.position}
                                ): Unit = {
    val frame = NetworkFrame(networks = networks,withLabel=withLabel,edgeColors=edgeColors,nodeColorClasses=nodeColorClasses,nodePositioning=nodePositioning)
    frame.init
  }

  def normalization(r: RasterLayerData[Double]): RasterLayerData[Double] = {
    val (mi,ma) = (r.flatten.min,r.flatten.max);r.map{_.map{d: Double => (d - mi) / (ma - mi)}}
  }

  case class RasterFrame(
                          raster: RasterLayerData[Double],
                          gradientColors: (Color,Color) = (Color.WHITE,Color.BLACK),
                          projection: RasterLayerData[Double] => RasterLayerData[Double] = normalization,
                          frameWidth: Int = 600,
                          frameHeight: Int = 600
  ) extends JFrame() {
    def init: Unit = {
      frameInit()
      setSize(frameWidth,frameHeight)
      setLocation(100,100)
      setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
      add(new JComponent {
        setUI(new ComponentUI {
          override def paint(g: Graphics, c: JComponent): Unit = {
            val gg: Graphics2D = g.create.asInstanceOf[Graphics2D]
            gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON)
            gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND,BasicStroke.JOIN_ROUND))
            val colorArray = projection(raster).map{_.map{d =>
              new Color(
              (gradientColors._1.getRed*d + (1 - d)*gradientColors._2.getRed).toInt,
              (gradientColors._1.getGreen*d + (1 - d)*gradientColors._2.getGreen).toInt,
              (gradientColors._1.getBlue*d + (1 - d)*gradientColors._2.getBlue).toInt
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

  def staticRasterVisualization(raster: RasterLayerData[Double],
                                projection: RasterLayerData[Double] => RasterLayerData[Double] = //normalization
                                 {r =>visualization.normalization(r.map{_.map{d => if(d<=0.0) 0.0 else scala.math.log10(d)}})}
                               ): Unit = {
    val frame = RasterFrame(raster)
    frame.init
  }



}
