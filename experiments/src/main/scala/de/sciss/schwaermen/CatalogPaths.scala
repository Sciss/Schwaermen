/*
 *  CatalogPaths.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen

import java.awt.geom.Line2D
import java.awt.{Color, RenderingHints}
import java.awt.image.BufferedImage
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.neuralgas
import de.sciss.neuralgas.{ComputeGNG, ImagePD}

import scala.swing.{Component, Dimension, Frame, Graphics2D, Swing}

object CatalogPaths {
  def main(args: Array[String]): Unit = {
    if (!fOutGNG.isFile) {
      runGNG()
    } else {
      println("(Skipping runGNG)")
    }
    viewGNG()
  }

  /*

    ok, here is the idea:

    - for each possible page-folding configuration:
    - create a GNG from the possible space occupied by the edges
      around the paragraphs
    - turn it into a MST
    - locate the possible starting and stopping points
    - find the path
    - create a bezier along it
    - make sure we don't overlap the paragraphs, otherwise
      rerun with stronger boundary padding
    - render the edge text to svg
    - create a place-on-path version of it
    - render it to PDF(s)

   */

  val ppi       : Double  = 150.0 // 90.0
  val ppmm      : Double  = ppi / 25.4
  val PaddingMM : Int     = 2

  lazy val PageWidthPx : Int = math.round(ppmm * Catalog.PaperWidthMM ).toInt
  lazy val PageHeightPx: Int = math.round(ppmm * Catalog.PaperHeightMM).toInt

  lazy val fOutGNG : File = Catalog.dir / "gng.bin"

  val GNG_COOKIE  = 0x474E470   // "GNG\0"

  case class Point2D(x: Double, y: Double)
  case class Edge(from: Int, to: Int)

  case class GraphGNG(surfaceWidth: Int, surfaceHeight: Int, nodes: Vec[Point2D], edges: Vec[Edge])

  def readGNG(): GraphGNG = {
    val fis = new FileInputStream(fOutGNG)
    try {
      val dis = new DataInputStream(fis)
      require (dis.readInt() == GNG_COOKIE)
      val surfaceWidthPx  = dis.readShort()
      val surfaceHeightPx = dis.readShort()
      val nNodes = dis.readInt()
      val nodes = Vector.fill(nNodes) {
        val x = dis.readFloat()
        val y = dis.readFloat()
        Point2D(x, y)
      }
      val nEdges = dis.readInt()
      val edges = Vector.fill(nEdges) {
        val from = dis.readInt()
        val to   = dis.readInt()
        Edge(from, to)
      }
      GraphGNG(surfaceWidthPx, surfaceHeightPx, nodes, edges)

    } finally {
      fis.close()
    }
  }

  def viewGNG(): Unit = {
    val gr = readGNG()
    Swing.onEDT {
      val comp = new Component {
        private[this] val _ln = new Line2D.Double

        override protected def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          val w     = peer.getWidth
          val h     = peer.getHeight
          val sx    = w.toDouble / gr.surfaceWidth
          val sy    = h.toDouble / gr.surfaceHeight
          val scale = math.min(sx, sy)
          g.setColor(Color.white)
          g.fillRect(0, 0, w, h)
          g.setColor(Color.black)
          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
          g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
          g.scale(scale, scale)
          val ln = _ln
          gr.edges.foreach { case Edge(from, to) =>
            val n1 = gr.nodes(from)
            val n2 = gr.nodes(to  )
            ln.setLine(n1.x, n1.y, n2.x, n2.y)
            g.draw(ln)
          }
        }
      }

//      val dx = math.max(1, gr.surfaceWidth  / 1280)
//      val dy = math.max(1, gr.surfaceHeight /  768)
//      val dp = math.max(dx, dy)

      new Frame {
        contents = comp
        preferredSize = new Dimension(1600, 400)
        pack().centerOnScreen()
        open()
      }
    }
  }

  def runGNG(): Unit = {
    val info                = Catalog.readOrderedParInfo()
    val SurfaceWidthPx      = PageWidthPx  * 6
    val SurfaceHeightPx     = PageHeightPx * 1
    println(s"surface = $SurfaceWidthPx x $SurfaceHeightPx")
    val img                 = new BufferedImage(SurfaceWidthPx, SurfaceHeightPx, BufferedImage.TYPE_BYTE_BINARY)
    val gImg                = img.createGraphics()
    gImg.setColor(Color.black)
    gImg.fillRect(0, 0, SurfaceWidthPx, SurfaceHeightPx)
    gImg.setColor(Color.white)
    info.foreach { i =>
      val pi  = Catalog.getParPage(i)
      val r   = Catalog.getParRectMM(i).border(PaddingMM) * ppmm
      val px  = pi * PageWidthPx
      val xi  = math.round(r.x).toInt
      val yi  = math.round(r.y).toInt
      val wi  = math.round(r.x + r.width ).toInt - xi
      val hi  = math.round(r.y + r.height).toInt - yi
      gImg.fillRect(xi + px, yi, wi, hi)
    }

    val c             = new ComputeGNG
    val pd            = new ImagePD(img, true)
    c.pd              = pd
    c.panelWidth      = img.getWidth  // / 8
    c.panelHeight     = img.getHeight // / 8
    c.maxNodes        = 8192; // 10000; // pd.getNumDots / 8
    // println(s"w ${compute.panelWidth}, h ${compute.panelHeight}, maxNodes ${compute.maxNodes}")
    c.stepSize        = 50
    c.algorithm       = neuralgas.Algorithm.GNGU
    c.lambdaGNG       = 100
    c.maxEdgeAge      = 88
    c.epsilonGNG      = 0.05f
    c.epsilonGNG2     = 1.0e-4f
    c.alphaGNG        = 0.2f
    c.setBetaGNG(5.0e-6f)
    c.noNewNodesGNGB  = false
    c.GNG_U_B         = true
    c.utilityGNG      = 8f
    c.autoStopB       = false
    c.reset()
    c.getRNG.setSeed(108L)
    c.addNode(null)
    c.addNode(null)

    val res             = new ComputeGNG.Result
    var lastNum         = 0
    var iter            = 0
    val t0              = System.currentTimeMillis()
    var lastT           = t0
    while (!res.stop && c.nNodes < c.maxNodes) {
      c.learn(res)
      if (c.nNodes != lastNum) {
        val t1 = System.currentTimeMillis()
        if (t1 - lastT > 4000) {
          lastNum = c.nNodes
          lastT   = t1
          println(lastNum)
        }
      }
      iter += 1
      //      if (iter == 1000) {
      //        println(compute.nodes.take(compute.nNodes).mkString("\n"))
      //      }
    }

    println(s"Done. Took ${(System.currentTimeMillis() - t0)/1000} seconds, and ${c.numSignals} signals.")
//    println(compute.nodes.take(compute.nNodes).mkString("\n"))

    val fos = new FileOutputStream(fOutGNG)
    try {
      val dos = new DataOutputStream(fos)
      dos.writeInt(GNG_COOKIE)
      dos.writeShort(SurfaceWidthPx)
      dos.writeShort(SurfaceHeightPx)
      dos.writeInt(c.nNodes)
      for (i <- 0 until c.nNodes) {
        val n = c.nodes(i)
        dos.writeFloat(n.x)
        dos.writeFloat(n.y)
      }
      dos.writeInt(c.nEdges)
      for (i <- 0 until c.nEdges) {
        val e = c.edges(i)
        dos.writeInt(e.from)
        dos.writeInt(e.to  )
      }

    } finally {
      fos.close()
    }
  }
}
