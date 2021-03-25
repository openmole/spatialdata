package org.openmole.spatialdata.utils.visualization

import java.awt.{BasicStroke, Color, Graphics, Graphics2D, RenderingHints}

import javax.swing.{JComponent, JFrame, WindowConstants}
import javax.swing.plaf.ComponentUI
import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.vector.{Attributes, Point, Polygons}

case class VectorFrame(
                        networks: Seq[Network]=Seq.empty,
                        frameWidth: Int = 600,
                        frameHeight: Int = 600,
                        nodeSize: Int = 6,
                        margin: Int = 50,
                        withLabel: Boolean = true,
                        edgeColoring: Link => Int = {_ => 0},
                        edgeScaling: Link => Double = {_ => 2.0},
                        nodeColoring: Node => Int = {_ => 0},
                        nodePositioning: Node => Point = {n => n.position},
                        nodeScaling: Node => Double = {_ => 1.0},
                        nodeShaping: Node => Int = {_ => 0},
                        polygons: Seq[Polygons]=Seq.empty,
                        polygonsScaleColoringAttributes: Seq[String] = Seq.empty
                      ) extends JFrame() {
  def init(): Unit = {
    frameInit()
    setSize(frameWidth+2*margin,frameHeight+2*margin)
    setLocation(100,100)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    add(new JComponent {
      setUI(new ComponentUI {
        override def paint(g: Graphics, c: JComponent): Unit = {
          // draw networks
          val gg = g.create.asInstanceOf[Graphics2D]
          NetworkFrame.drawNetwork(gg,
            networks, frameWidth, frameHeight, nodeSize, margin, withLabel, edgeColoring, edgeScaling, nodeColoring, nodePositioning, nodeScaling, nodeShaping
          )
          // draw polygons
          VectorFrame.drawPolygons(gg, polygons, polygonsScaleColoringAttributes.map(attrName => VectorFrame.attributeGradientScale(attrName)))
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


object VectorFrame {

  /**
    * draw some polygons
    * @param gg graphic
    * @param polygons polygons
    * @param attributeScaleColoring scale coloring according to an attribute value
    */
  def drawPolygons(gg: Graphics2D,
                   polygons: Seq[Polygons],
                   attributeScaleColoring: Seq[Attributes => Color]
                  ): Unit = {
    gg.setColor(Color.BLACK)
    gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

    polygons.zip(attributeScaleColoring).foreach{case (polys,coloring) =>
      polys.polygons.zip(polys.attributes).foreach{case (poly,attr) =>
        gg.setColor(coloring(attr))
        gg.fillPolygon(poly.getCoordinates.map(_.x),poly.getCoordinates.map(_.y),poly.getCoordinates.length)
      }
    }

  }

  /**
    * gradient scale given a double attribute
    * @param attrName name of the attribute
    * @param gradientColors gradient color
    * @param a attribute
    * @return
    */
  def attributeGradientScale(attrName: String, gradientColors: (Color,Color) = (Color.LIGHT_GRAY, Color.BLACK))(a: Attributes): Color = {
    val d = a.getOrElse(attrName,0.0).asInstanceOf[Double]
    new Color(
      (gradientColors._1.getRed*(1 - d) + d*gradientColors._2.getRed).toInt,
      (gradientColors._1.getGreen*(1 - d) + d*gradientColors._2.getGreen).toInt,
      (gradientColors._1.getBlue*(1 - d) + d*gradientColors._2.getBlue).toInt
    )
  }

}
