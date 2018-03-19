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

import java.awt.geom.{AffineTransform, Line2D}
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints}
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.catmullrom.{CatmullRomSpline, Point2D}
import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.neuralgas
import de.sciss.neuralgas.{ComputeGNG, ImagePD}
import de.sciss.numbers
import de.sciss.schwaermen.Catalog.{ColumnSepMM, FontSizePt, LineSpacingPt, Transform, dirTmp, exec, inkscape, pdflatex, setAttr, stripTemplate, writeSVG, ppmmSVG, writeText, PaperWidthMM, PaperHeightMM}

import scala.swing.{Component, Dimension, Frame, Graphics2D, Swing}

object CatalogPaths {
  def main(args: Array[String]): Unit = {
    if (!fOutGNG.isFile) {
      runGNG()
    } else {
      println("(Skipping runGNG)")
    }
//    viewGNG()
    test()
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

  val ppi       : Double  = 150.0       // for the GNG
  val ppmm      : Double  = ppi / 25.4  // for the GNG
  val PadParMM  : Int     = 5 // 2
  val PadMarMM  : Int     = 5 // 2

  lazy val PageWidthPx  : Int = math.round(ppmm * PaperWidthMM ).toInt
  lazy val PageHeightPx : Int = math.round(ppmm * PaperHeightMM).toInt

//  lazy val PageIWidthPx : Int = math.round(ppmm * PaperIWidthMM).toInt
//  lazy val PageIHeightPx: Int = PageHeightPx

  lazy val fOutGNG : File = Catalog.dir / "gng.bin"

  val GNG_MaxNodes  = 6561 // 8192

  private val GNG_COOKIE = 0x474E470   // "GNG\0"

  case class Edge(from: Int, to: Int) {
    def swap = Edge(to, from)
  }

  case class GraphGNG(surfaceWidth: Int, surfaceHeight: Int, nodes: Vec[Point2D], edges: Vec[Edge])

  def latexEdgeTemplate(text: String): String = stripTemplate(
    s"""@documentclass[10pt]{article}
       |@usepackage[paperheight=${LineSpacingPt * 3 / ppmm}mm,paperwidth=300mm,top=0mm,bottom=0mm,right=0mm,left=0mm]{geometry}
       |@usepackage[ngerman]{babel}
       |@usepackage{Alegreya}
       |@usepackage[T1]{fontenc}
       |@usepackage[utf8]{inputenc}
       |@setlength{@columnsep}{${ColumnSepMM}mm}
       |@begin{document}
       |@pagestyle{empty}
       |@fontsize{${FontSizePt}pt}{${LineSpacingPt}pt}@selectfont
       |@noindent
       |$text
       |@end{document}
       |"""
  )

  val testRoute: List[Int] = List(
    3621, 2999, 6374,  182, 3149, 1398,  933, 2136, 1413, 4812,  124,  635, 4662, 1373, 3859, 4332, 1615, 3705, 2496,
    3654,  502, 5303,  340,  417, 4416, 6510, 2647, 3996, 5986,  961, 3244,  121, 4740, 3088, 4457, 1982, 2758, 1842,
    6007, 2731, 1748, 1897, 2826, 4156, 6072, 2789, 1294, 6127,  460, 3418, 2323, 3513, 2536, 1718, 2201, 3301, 2239,
    5189, 1520, 5137, 1530,  690, 5543, 3535,  467, 5297, 2374, 3603, 5490,  132,  847, 4444,  566, 3235,   90,   97,
    2561, 5624,  800,  367, 1860, 5199, 2926, 1926, 4377, 2925, 6317, 4372, 1333,  927, 3133, 2096, 4749,  620,  945,
    4796, 2159,  274,  423, 5469, 2528, 3977, 6143, 1695, 1376, 3116,  399, 4447,  163, 1931, 6452, 2867, 4246, 6323,
     553, 2674, 5816, 1211, 3998, 6008,  356, 2491, 3724, 1128,   10, 5209, 3176, 4098, 6149,   70, 6060, 1213, 1788,
    2643, 6147, 3959, 1768, 5914, 1200, 1813, 6061, 4036,   14, 2329, 1037,  832, 1250, 2907, 4380,  550,  365, 2773,
    4179, 1853, 6252,  539, 2777, 4196, 1216, 6242,  528, 4132, 6354, 1179, 2651,   44, 3607, 1587, 5327,   28, 2412,
    6082, 4742, 3130, 1383,  619,  957, 3274, 4919, 3278, 1503, 5063, 2098, 4784, 1397,  207, 2524, 1116, 3888, 1696,
      38, 5842, 3902, 2574,  320, 2504, 5869, 1649, 2470, 3689, 5660,  135, 3073,  577, 1365, 5470, 3609, 2370,  296,
     441, 4866, 1500
  )
  
  class PathCursor(pt: Vec[Point2D]) {
    private[this] var _pos  = 0.0
    private[this] var ptIdx = 0
    private[this] var ptOff = 0.0
    
    lazy val extent: Double =
      pt.sliding(2, 1).map { case Seq(a, b) => a dist b } .sum

    def pos: Double = _pos

    def advance(amt: Double): Unit = {
      var rem = amt
      while (rem > 0) {
        val c   = location
        val p1  = pt(ptIdx1)
        val d   = c dist p1
        if (rem < d) {
          _pos  += rem
          rem    = 0.0
        } else {
          _pos  += d
          ptIdx  = math.min(pt.size - 1, ptIdx + 1)
          ptOff  = _pos
          rem   -= d
        }
      }
    }

    private def ptIdx1 = math.min(pt.size - 1, ptIdx + 1)

    private def currLen: Double = {
      val p0  = pt(ptIdx)
      val p1  = pt(ptIdx1)
      p0 dist p1
    }

    import numbers.Implicits._

    def location: Point2D = {
      val p0  = pt(ptIdx)
      val p1  = pt(ptIdx1)
      val len = currLen
      val x   = _pos.linlin(ptOff, ptOff + len, p0.x, p1.x)
      val y   = _pos.linlin(ptOff, ptOff + len, p0.y, p1.y)
      Point2D(x, y)
    }

    def rotation: Double = {
      val p0  = pt(ptIdx)
      val p1  = pt(ptIdx1)
      val d   = p1 + (p0 * -1)
      math.atan2(d.y, d.x)
    }

    def transform(scale: Double = 1.0): Transform = {
      val loc = location
      val r   = rotation
      val at  = AffineTransform.getTranslateInstance(loc.x * scale, loc.y * scale)
      at.rotate(r)
      Transform.fromAwt(at)
    }
  }

  def test(): Unit = {
    val text = CatalogTexts.edgesDe.head._2
    val latex = latexEdgeTemplate(text)
    require (dirTmp.isDirectory, s"Not a directory: $dirTmp")
    val fOutTex = dirTmp / s"edge_temp.tex"
    val fOutPDF = fOutTex.replaceExt("pdf")
    writeText(latex, fOutTex)

    val argPDF = Seq("-interaction=batchmode", fOutTex.path)
    exec(pdflatex, dirTmp, argPDF)

    val fOutSVG = fOutPDF.replaceExt("svg")
    val argSVG = Seq("-l", fOutSVG.path, fOutPDF.path)
    exec(inkscape, dirTmp, argSVG)

    val svg = Catalog.parseSVG(fOutSVG)

    val gr    = readGNG()
    val route = testRoute
    val intp: Vec[Point2D] = route.grouped(3).map(_.head).sliding(4).flatMap { nodeIds =>
      val Seq(p1, p2, p3, p4) = nodeIds.map { id =>
        val n = gr.nodes(id)
        Point2D(n.x / ppmm, n.y / ppmm)
      }
      CatmullRomSpline(CatmullRomSpline.Chordal, p1, p2, p3, p4).calcN(8)
    } .toIndexedSeq

    val textNode    = svg.text.head
    val textLine    = textNode.children.head
    val textScaleMM = svg.transform.scaleX / Catalog.ppmmSVG
    val textExtent  = textLine.x.last * textScaleMM
    val pathCursor  = new PathCursor(intp)
    val pathExtent  = pathCursor.extent // intp.sliding(2, 1).map { case Seq(a, b) => (a dist b) / ppmm } .sum

    println(f"textExtent = $textExtent%1.1fmm, pathExtent = $pathExtent%1.1fmm.")

    val chopped     = textNode.chop(0)
    var lastTextX   = 0.0
    val textNodesOut = chopped.map { t =>
      val x0 = t.children.head.x.head
      pathCursor.advance((x0 - lastTextX) * textScaleMM)
      lastTextX = x0
      val t1 = t.mapChildren { tSpan =>
        tSpan.setLocation(0.0 :: Nil, 0.0)
      }
      val t2 = t1.setTransform(pathCursor.transform(1.0 / textScaleMM))
      t2
    }

    val widthOut  = PaperWidthMM  * ppmmSVG
    val heightOut = PaperHeightMM * ppmmSVG

    val gTransOut = {
//      val at = AffineTransform.getTranslateInstance(0.0, heightOut)
//      at.scale(1.0 / Catalog.ppmSVG, -1.0 / Catalog.ppmSVG)
      val at = AffineTransform.getTranslateInstance(0.0, 0.0)
      // at.scale(Catalog.ppmSVG, Catalog.ppmSVG)
      at.scale(svg.transform.scaleX, svg.transform.scaleX)
      Transform.fromAwt(at)
    }

    val svg1 = {
      val s0 = svg.setTransform(gTransOut)
      val d0 = setAttr(s0.doc, "width"  , widthOut .toString)
      val d1 = setAttr(d0    , "height" , heightOut.toString)
      val d2 = setAttr(d1    , "viewBox", s"0 0 $widthOut $heightOut")
      s0.copy(doc = d2)
    }

    val svg2 = {
      val g1 = svg1.group .copy(child = textNodesOut.map(_.node))
      val d1 = svg1.doc   .copy(child = g1 :: Nil)
      svg1.copy(text = textNodesOut, group = g1, doc = d1)
    }

    val fOutSVG2 = fOutSVG.parent / s"${fOutSVG.base}-out.svg"
    writeSVG(svg2.doc, fOutSVG2)

    val fOutPDF2 = fOutSVG2.replaceExt("pdf")
    val argsSVG2 = Seq("--export-pdf", fOutPDF2.path, fOutSVG2.path)
    exec(inkscape, dirTmp, argsSVG2)
  }

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
//    val startIdx  = util.Random.nextInt(gr.nodes.size)
//    val stopIdx   = {
//      val i = util.Random.nextInt(gr.nodes.size - 1)
//      if (i >= startIdx) i + 1 else i
//    }
    val rnd       = new util.Random(108L)
    val left      = gr.nodes.iterator.zipWithIndex.filter(_._1.x < gr.surfaceWidth * 1/3).map(_._2).toIndexedSeq
    val right     = gr.nodes.iterator.zipWithIndex.filter(_._1.x > gr.surfaceWidth * 2/3).map(_._2).toIndexedSeq
    val startIdx  = left (rnd.nextInt(left .size))
    val stopIdx   = right(rnd.nextInt(right.size))
    println(s"startIdx $startIdx, stopIdx $stopIdx")

//    val nDijk = gr.nodes.zipWithIndex.map { case (pt, idx) =>
//      dijkstra.Node(idx, x = pt.x, y = pt.y)
//    }
//    val eDijk = gr.edges.iterator.map { e =>
//      dijkstra.Edge(e.from, e.to)
//    } .toList
//    val gDijk = dijkstra.Graph(nDijk.iterator.zipWithIndex.map(_.swap).toMap, eDijk)

//    val t0 = System.currentTimeMillis()
//    val dijkstra.ShortestRoute(route, routeDist) = gDijk.shortestPath(startIdx, stopIdx)
//    val t1 = System.currentTimeMillis()
//    println(s"Shortest route done: ${route.size - 1} steps, $routeDist total distance. Took ${(t1-t0)/1000} seconds")
//    println(route.mkString("[", ", ", "]"))
    val route = testRoute

//    val routeE = route.sliding(2, 1).map { case Seq(from, to) => Edge(from, to) }.toList
//
//    val ok = routeE.forall(e => gr.edges.contains(e) || gr.edges.contains(e.swap))
//    println(s"ALL EDGES IN SET? $ok")

    // 'smooth' by sub-sampling with factor 3 before going into the spline interp
    val intp: Array[Point2D] = route.grouped(3).map(_.head).sliding(4).flatMap { nodeIds =>
      val Seq(p1, p2, p3, p4) = nodeIds.map { id =>
        val n = gr.nodes(id)
        Point2D(n.x, n.y)
      }
      CatmullRomSpline(CatmullRomSpline.Chordal, p1, p2, p3, p4).calcN(8)
    } .toArray

    Swing.onEDT {
      val comp = new Component {
        private[this] val _ln       = new Line2D.Double
        private[this] val strkThick = new BasicStroke(3f)

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

          def drawEdges(sq: Seq[Edge]): Unit =
            sq.foreach { case Edge(from, to) =>
              val n1 = gr.nodes(from)
              val n2 = gr.nodes(to  )
              ln.setLine(n1.x, n1.y, n2.x, n2.y)
              g.draw(ln)
            }

          drawEdges(gr.edges)
          g.setColor(Color.red)
          val strkOrg = g.getStroke
          g.setStroke(strkThick)
//          drawEdges(routeE)

          intp.sliding(2, 1).foreach { case Array(n1, n2) =>
            ln.setLine(n1.x, n1.y, n2.x, n2.y)
            g.draw(ln)
          }

          g.setStroke(strkOrg)
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
    gImg.setColor(Color.white)
    gImg.fillRect(0, 0, SurfaceWidthPx, SurfaceHeightPx)
    gImg.setColor(Color.black)
    val MarPx = math.round(PadMarMM * ppmm).toInt
    gImg.fillRect(MarPx, MarPx, SurfaceWidthPx - (MarPx + MarPx), SurfaceHeightPx - (MarPx + MarPx))
    gImg.setColor(Color.white)
    info.zipWithIndex.foreach { case (i, parIdx) =>
      val pi  = Catalog.getParPage(parIdx)
      val r   = Catalog.getParRectMM(info, parIdx = parIdx).border(PadMarMM) * ppmm
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
    c.maxNodes        = GNG_MaxNodes; // 10000; // pd.getNumDots / 8
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
