package org.openmole.spatialdata.utils.visualization

import java.awt._

import javax.swing.{JComponent, JFrame}
import javax.swing.plaf.ComponentUI
import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.vector.Point

case class NetworkFrame(networks: Seq[Network],
                        frameWidth: Int = 600,
                        frameHeight: Int = 600,
                        nodeSize: Int = 6,
                        margin: Int = 50,
                        withLabel: Boolean = true,
                        edgeColorClasses: Link => Int = {_ => 0},
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

          networks.foreach {network =>
            network.nodes.foreach { n => {
              val np = nodePositioning(n)
              val (x, y) = (margin + (np._1 * frameWidth - nodeSize / 2).toInt, margin + (np._2 * frameHeight - nodeSize / 2).toInt)
              //if (nodeColorClasses.isDefined) gg.setColor(palette(nodeColorClasses.get(n))) //slight perf loss without option?
              gg.setColor(palette(nodeColorClasses(n)))
              gg.fillRect(x, y, nodeSize, nodeSize)
              if (withLabel) gg.drawString(n.id.toString, x + nodeSize, y + nodeSize)
            }
            }

            network.links.foreach { l =>
              gg.setColor(palette(edgeColorClasses(l)))
              gg.drawLine(
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
