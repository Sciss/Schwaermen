/*
 *  CatalogCover.scala
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
import java.awt.{BasicStroke, Color, RenderingHints}
import java.awt.image.BufferedImage
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.catmullrom.Point2D
import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.neuralgas
import de.sciss.neuralgas.ComputeGNG
import de.sciss.schwaermen.CatalogPaths.GNG_COOKIE
import javax.imageio.ImageIO

object CatalogCover {
  def main(args: Array[String]): Unit = {
    val fIn   = file("/data/projects/Schwaermen/photos/170826_Naya/_MG_9905_rot_2zu1gray.jpg")
    val fGNG  = file("/data/temp/catalog_cover.gng")
    if (fGNG.exists() && fGNG.length() > 0L) {
      println(s"GNG file $fGNG already exists. Not overwriting.")
    } else {
      val img   = ImageIO.read(fIn)
      runGNG(img = img, fOutGNG = fGNG)
    }

    val fImgOut = file("/data/temp/catalog_cover.png")
    if (fImgOut.exists() && fImgOut.length() > 0L) {
      println(s"Image file $fImgOut already exists. Not overwriting.")
    } else {
      render(fGNG, fImgOut)
    }
  }

  def render(fGNG: File, fImgOut: File): Unit = {
    val graph = readGNG(fGNG)
    val w     = graph.surfaceWidthPx
    val h     = graph.surfaceHeightPx
    val img   = new BufferedImage(w, h, BufferedImage.TYPE_BYTE_GRAY)
    val g     = img.createGraphics()
    g.setColor(Color.white)
    g.fillRect(0, 0, w, h)
    g.setColor(Color.black)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON   )
    g.setRenderingHint(RenderingHints.KEY_RENDERING     , RenderingHints.VALUE_RENDER_QUALITY )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE    )
    g.setStroke(new BasicStroke(2f))
    val ln = new Line2D.Double()
    graph.edges.foreach { case CatalogPaths.Edge(from, to) =>
      val nFrom = graph.nodes(from)
      val nTo   = graph.nodes(to  )
      ln.setLine(nFrom.x, nFrom.y, nTo.x, nTo.y)
      g.draw(ln)
    }
    g.dispose()
    val fmt   = if (fImgOut.extL == "png") "png" else "jpg"
    ImageIO.write(img, fmt, fImgOut)
  }

  case class ResGNG(surfaceWidthPx: Int, surfaceHeightPx: Int, nodes: Vec[Point2D],
                    edges: Vec[CatalogPaths.Edge])

  def readGNG(fGNG: File): ResGNG = {
    val fis = new FileInputStream(fGNG)
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
        CatalogPaths.Edge(from, to)
      }
      ResGNG(surfaceWidthPx, surfaceHeightPx, nodes, edges)

    } finally {
      fis.close()
    }
  }

  def runGNG(img: BufferedImage, fOutGNG: File, rngSeed: Long = 0xBEE): Unit = {
    val c = new ComputeGNG
    val pd = new GrayImagePD(img, true)
    c.pd = pd
    val w = img.getWidth
    val h = img.getHeight
    c.panelWidth  = w // / 8
    c.panelHeight = h // / 8
    c.maxNodes = pd.getNumPixels / 108
    println(s"w ${c.panelWidth}, h ${c.panelHeight}, maxNodes ${c.maxNodes}")
    c.stepSize = 50
    c.algorithm = neuralgas.Algorithm.GNGU
    c.lambdaGNG = 100
    c.maxEdgeAge = 88
    c.epsilonGNG = 0.05f
    c.epsilonGNG2 = 1.0e-4f
    c.alphaGNG = 0.2f
    c.setBetaGNG(5.0e-6f)
    c.noNewNodesGNGB = false
    c.GNG_U_B = true
    c.utilityGNG = 8f
    c.autoStopB = false
    c.reset()
    //    c.getRNG.setSeed(108L)
    c.getRNG.setSeed(rngSeed)
    c.addNode(null)
    c.addNode(null)

    val res = new ComputeGNG.Result
    var lastProg = 0
    var iter = 0
    val t0 = System.currentTimeMillis()
    println("_" * 100)
    while (!res.stop && c.nNodes < c.maxNodes) {
      c.learn(res)
      val prog: Int = (c.nNodes * 100) / c.maxNodes
      if (lastProg < prog) {
        while (lastProg < prog) {
          print('#')
          lastProg += 1
        }
        if (prog % 5 == 0) {
          val fTemp = fOutGNG.parent / s"${fOutGNG.base}-$prog.${fOutGNG.ext}"
          writeGNG(c, fTemp, w = w, h = h)
        }
      }
      iter += 1
      //      if (iter == 1000) {
      //        println(compute.nodes.take(compute.nNodes).mkString("\n"))
      //      }
    }

    println(s" Done. Took ${(System.currentTimeMillis() - t0) / 1000} seconds.") // ", and ${c.numSignals} signals."
    //    println(compute.nodes.take(compute.nNodes).mkString("\n"))
    writeGNG(c, fOutGNG, w = w, h = h)
  }

  def writeGNG(c: ComputeGNG, fOut: File, w: Int, h: Int): Unit = {
    val fos = new FileOutputStream(fOut)
    try {
      val dos = new DataOutputStream(fos)
      dos.writeInt(GNG_COOKIE)
      dos.writeShort(w)
      dos.writeShort(h)
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
