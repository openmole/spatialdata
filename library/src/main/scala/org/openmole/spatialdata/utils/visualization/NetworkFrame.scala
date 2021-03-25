package org.openmole.spatialdata.utils.visualization

import java.awt._

import javax.swing.{JComponent, JFrame, WindowConstants}
import javax.swing.plaf.ComponentUI
import org.openmole.spatialdata.network.{Link, Network, Node}
import org.openmole.spatialdata.vector.Point

case class NetworkFrame(networks: Seq[Network],
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
                        nodeShaping: Node => Int = {_ => 0}
                       ) extends JFrame() {
  def init(): Unit = {
    frameInit()
    setSize(frameWidth+2*margin,frameHeight+2*margin)
    setLocation(100,100)
    setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
    add(new JComponent {
      setUI(new ComponentUI {
        override def paint(g: Graphics, c: JComponent): Unit = {
          NetworkFrame.drawNetwork(g.create.asInstanceOf[Graphics2D],
            networks, frameWidth, frameHeight, nodeSize, margin, withLabel, edgeColoring, edgeScaling, nodeColoring, nodePositioning, nodeScaling, nodeShaping
          )
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


object NetworkFrame {


  def drawNetwork(gg: Graphics2D,
                  networks: Seq[Network],
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
                  nodeShaping: Node => Int = {_ => 0}
                 ): Unit = {
    gg.setColor(Color.BLACK)
    gg.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    gg.setStroke(new BasicStroke(1, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))

    networks.foreach {network =>
      network.nodes.foreach { n => {
        val np = nodePositioning(n)
        val ns = (nodeSize*nodeScaling(n)).toInt
        val nshape = nodeShaping(n)
        val (x, y) = (margin + (np._1 * frameWidth - ns / 2).toInt, margin + (np._2 * frameHeight - ns / 2).toInt)
        //if (nodeColorClasses.isDefined) gg.setColor(palette(nodeColorClasses.get(n))) //slight perf loss without option?
        gg.setColor(palette(nodeColoring(n)))
        nshape match {
          case 0 => gg.fillRect(x, y, ns, ns)
          case 1 => gg.fillOval(x, y, ns, ns)
          case _ => gg.fillRect(x, y, ns, ns)
        }
        if (withLabel) gg.drawString(n.id.toString, x + ns, y + ns)
      }
      }

      network.links.foreach { l =>
        val esize = edgeScaling(l)
        gg.setStroke(new BasicStroke(esize.toFloat, BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND))
        gg.setColor(palette(edgeColoring(l)))
        val (x0,y0,x1,y1) = (margin + (nodePositioning(l.e1)._1 * frameWidth).toInt,margin + (nodePositioning(l.e1)._2 * frameHeight).toInt, margin + (nodePositioning(l.e2)._1 * frameWidth).toInt, margin + (nodePositioning(l.e2)._2 * frameHeight).toInt)
        //val n = math.sqrt(math.pow(x1-x0,2.0)+math.pow(y1-y0,2.0))
        //val (ux,uy) = ((y0-y1).toDouble/n, (x1-x0).toDouble/n)
        //(-esize/2 until esize/2).foreach {i =>
        //  gg.drawLine((x0 + i*ux).toInt, (y0 + i*uy).toInt, (x1 + i*ux).toInt, (y1 + i*uy).toInt)
        //}
        // better use stroke setting
        if (esize>0.0) gg.drawLine(x0 , y0, x1, y1)
      }
    }
  }

}
