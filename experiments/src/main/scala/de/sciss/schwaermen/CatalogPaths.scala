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
import de.sciss.neuralgas.{ComputeGNG, ImagePD}
import de.sciss.schwaermen.Catalog._
import de.sciss.swingplus.{ComboBox, Spinner}
import de.sciss.{neuralgas, numbers}
import javax.swing.SpinnerNumberModel

import scala.swing.event.{SelectionChanged, ValueChanged}
import scala.swing.{BorderPanel, Component, Dimension, Frame, Graphics2D, GridPanel, Label, Swing}

object CatalogPaths {
  def main(args: Array[String]): Unit = {
    val info  = readOrderedParInfo()
    val folds = mkFoldSeq(info, Lang.De)
//    folds.foreach { fold =>
//      val fOutGNG = gngFile(fold)
//      if (!fOutGNG.isFile) {
//        println(s"runGNG(${fOutGNG.name})")
//        runGNG(info, fold, fOutGNG)
//      }
//    }

//    testDrawPath(folds(6))
//    viewGNG(folds(5))
    testViewFolds()
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

  lazy val ppi       : Double  = 150.0       // for the GNG
  lazy val ppmm      : Double  = ppi / 25.4  // for the GNG
  lazy val PadParMM  : Int     = 5 // 2
  lazy val PadMarMM  : Int     = 5 // 2

  lazy val PageWidthPx  : Int = math.round(ppmm * PaperWidthMM ).toInt
  lazy val PageHeightPx : Int = math.round(ppmm * PaperHeightMM).toInt

//  lazy val PageIWidthPx : Int = math.round(ppmm * PaperIWidthMM).toInt
//  lazy val PageIHeightPx: Int = PageHeightPx

//  lazy val fOutGNG : File = Catalog.dir / "gng.bin"

  lazy val GNG_MaxNodes  = 6561 // 8192

  private val GNG_COOKIE = 0x474E470   // "GNG\0"

  case class Edge(from: Int, to: Int) {
    def swap = Edge(to, from)
  }

  case class GraphGNG(fold: Fold, nodes: Vec[Point2D], edges: Vec[Edge]) {
    lazy val surfaceWidthPx : Int = (fold.surfaceMM.width  * ppmm).toInt
    lazy val surfaceHeightPx: Int = (fold.surfaceMM.height * ppmm).toInt
  }

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

  lazy val testRoute: List[Int] = List(
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

  lazy val foldIndices: List[List[Int]] = {
    val base = 0 until (NumPages - 1)
    val res = (0 to NumPages/2).toList.flatMap { nc =>
      val comb = base.toList.combinations(nc)
//      val filtered = comb.filterNot { sq =>
//        sq.sliding(2, 1).exists { case Seq(a, b) => a + 1 == b; case _ => false }
//      }
      val filtered = comb
      filtered.toList
    } .distinct
    res
  }

  case class FoldPage(idx: Int, rectangle: Rectangle, par: List[Rectangle], rotation: Int = 0) {
    require(par.forall(rectangle.contains), toString)

    def isCW      : Boolean = rotation == +180
    def isCCW     : Boolean = rotation == -180
    def isUpright : Boolean = rotation ==    0

    def shift(dx: Double, dy: Double): FoldPage = {
      val r1    = rectangle.translate(dx, dy)
      val par1  = par.map(_.translate(dx, dy))
      copy(rectangle = r1, par = par1)
    }

    private def rotatePars: List[Rectangle] = par.map { r =>
      val x1 = rectangle.right  - r.right  + rectangle.left
      val y1 = rectangle.bottom - r.bottom + rectangle.top
      val dx = x1 - r.x
      val dy = y1 - r.y
      r.translate(dx, dy)
    }

    def rotateCW: FoldPage = {
      require(!isCW)
      val rot1 = rotation + 180
      performRotate(rot1, isCCW = false)
    }

    def rotateCCW: FoldPage = {
      require(!isCCW)
      val rot1 = rotation - 180
      performRotate(rot1, isCCW = true)
    }

    private def performRotate(rot1: Int, isCCW: Boolean): FoldPage = {
      val par1 = rotatePars
      // in CW , if rotation was    0, bottom left corner is fixed - translation is (-w, +h)
      // in CW , if rotation was -180, bottom left corner is fixed - translation is (+w, -h
      // in CCW, if rotation was    0, top    left corner is fixed - translation is (-w, -h)
      // in CCW, if rotation was -180, top    left corner is fixed - translation is (+w, +h
      val dx    = if (isUpright)         -rectangle.width   else +rectangle.width
      val dy    = if (isUpright ^ isCCW) +rectangle.height  else -rectangle.height
      val f1    = copy(par = par1, rotation = rot1)
      f1.shift(dx, dy)
    }

    def nextTo(pred: FoldPage): FoldPage = {
      require(isUpright)
      if (pred.isUpright) {
        val curr = this
        val dx = pred.rectangle.right - curr.rectangle.left
        val dy = pred.rectangle.top   - curr.rectangle.top
        curr.shift(dx, dy)

      } else {
        val curr = if (pred.isCW) rotateCW else rotateCCW
        val dx = pred.rectangle.left  - curr.rectangle.right
        val dy = pred.rectangle.top   - curr.rectangle.top
        curr.shift(dx, dy)
      }
    }
  }

  case class Fold(pages: List[FoldPage]) {
    lazy val surfaceMM: Rectangle = {
      val r = pages.map(_.rectangle)
      val hd :: tl = r
      tl.foldLeft(hd)(_ union _)
    }

    lazy val rotations: List[Int] = pages.map(_.rotation)

    lazy val rotationString         : String = mkRotString(pageIndices = false)
    lazy val pagesAndRotationsString: String = mkRotString(pageIndices = true )

    def id: String = pagesAndRotationsString

    private def mkRotString(pageIndices: Boolean): String =
      pages.iterator.foldLeft(List.empty[String] -> Option.empty[FoldPage]) { case ((s, pPrev), p) =>
        val rPrev = pPrev.fold(0)(_.rotation)
        val s0 =
          if (p.rotation == rPrev) "s"
          else if (p.rotation < rPrev) "U"
          else "D"
        val s2 = pPrev.fold(s) { prev =>
          List.fill(p.idx - prev.idx - 1)("X") ::: s
        }
        val s1 = if (pageIndices) s"${p.idx}$s0" else s0
        (s1 :: s2, Some(p))
      }._1.reverse.mkString("-")

    // makes sure first page is upright and union rectangle has origin of (0, 0).
    def normalize: Fold = {
      val r0 = pages.headOption.fold(0)(_.rotation)
      val f1 = if (r0 == 0) this else {
        // val action: FoldPage => FoldPage = if (r0 < 0) _.rotateCW else _.rotateCCW
        val pages1 = pages.foldLeft(List.empty[FoldPage]) { (res, p) =>
          val rOld  = p.rotation
          val rNew  = (rOld - r0 + 360) % 360
          val p0    = if (p.isCW) p.rotateCCW else if (p.isCCW) p.rotateCW else p
          val p1    = res.headOption.fold(p0)(p0.nextTo)
          val p2    = if (rNew < 0) p1.rotateCCW else if (rNew > 0) p1.rotateCW else p1
          p2 :: res
        } .reverse
        copy(pages = pages1)
      }

      val s   = f1.surfaceMM
      val dx  = -s.x
      val dy  = -s.y
      val p1  = f1.pages.map(_.shift(dx, dy))
      f1.copy(pages = p1)
    }

    def isValid: Boolean = pages.nonEmpty && {
      pages.combinations(2).forall { case Seq(a, b) => !(a.rectangle overlaps b.rectangle) }
    }
  }

  def mkFoldSeq(info: Vec[ParFileInfo], lang: Lang): List[Fold] = {
    val langFoldWay = if (lang == Lang.De) 1 else 0
    val all = foldIndices.flatMap { indices =>
      def loop(idx: Int, rem: List[Int], pred: List[FoldPage], res: List[Fold]): List[Fold] =
        if (idx == NumPages) {
          val f = Fold(pages = pred)
          if (f.isValid && f.pages.head.idx == 0 && f.pages.last.idx == NumPages - 1) res :+ f else res
        } else {
          val r1      = getPageRectMM(pageIdx = idx, absLeft = true, lang = lang)
          val parIdx0 = idx * 3
          val par1    = List(parIdx0, parIdx0+1, parIdx0+2).map(parIdx =>
            getParRectMM(info, parIdx = parIdx, absLeft = true, lang = lang)
          )
          val p0      = FoldPage(idx = idx, rectangle = r1, par = par1)
          val p1      = pred.lastOption.fold(p0)(p0.nextTo)
          val foldIdx = idx - 1
          rem match {
            case `foldIdx` :: remTail =>
              if (foldIdx % 2 == langFoldWay) { // rotate; == 0 for En, == 1 for De
                val res1 = if (p1.isCCW) res else {
                  val p2 = p1.rotateCCW
                  loop(idx = idx + 1, rem = remTail, pred = pred :+ p2, res = res)
                }
                if (p1.isCW) res1 else {
                  val p2 = p1.rotateCW
                  loop(idx = idx + 1, rem = remTail, pred = pred :+ p2, res = res1)
                }

              } else {  // hide
                loop(idx = idx + 1, rem = remTail, pred = pred.init, res = res)
              }

            case _ => loop(idx = idx + 1, rem = rem, pred = pred :+ p1, res = res)
          }
        }

      val seq0 = loop(idx = 0, rem = indices, pred = Nil, res = Nil)
      seq0.map(_.normalize)
    }
    distinctFolds(all)
  }

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

  def testDrawPath(fold: Fold): Unit = {
    val text = CatalogTexts.edgesDe.head._2
    val latex = latexEdgeTemplate(text)
    require (dirTmp.isDirectory, s"Not a directory: $dirTmp")
    val fOutTex = dirTmp / s"edge_${fold.rotationString}.tex"
    val fOutPDF = fOutTex.replaceExt("pdf")
    writeText(latex, fOutTex)

    val argPDF = Seq("-interaction=batchmode", fOutTex.path)
    exec(pdflatex, dirTmp, argPDF)

    val fOutSVG = fOutPDF.replaceExt("svg")
    val argSVG = Seq("-l", fOutSVG.path, fOutPDF.path)
    exec(inkscape, dirTmp, argSVG)

    val svg = Catalog.parseSVG(fOutSVG)

    val gr    = readGNG(fold)
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

    val widthOut  = TotalPaperWidthMM /* PaperWidthMM */ * ppmmSVG
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

  def gngFile(fold: Fold): File =
    Catalog.dir / s"gng-${fold.rotationString}.bin"

  def readGNG(fold: Fold): GraphGNG = {
    val fOutGNG = gngFile(fold)
    val fis = new FileInputStream(fOutGNG)
    try {
      val dis = new DataInputStream(fis)
      require (dis.readInt() == GNG_COOKIE)
      /* val surfaceWidthPx  = */ dis.readShort()
      /* val surfaceHeightPx = */ dis.readShort()
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
      GraphGNG(fold, nodes, edges)

    } finally {
      fis.close()
    }
  }

  /** Selects only valid folds for a given path.
    * Validity:
    *
    * - contains page for source, contains page for target
    * - if source paragraph is on the right column (middle of three paragraphs),
    *   page must be succeeded to the right (not up or down)
    * - if target paragraph is on the left column (top or bottom of three paragraphs),
    *   page must be preceded to the left (not up or down)
    *
    * The result is then trimmed so pages start with source page
    * and end with target page, and made distinct.
    */
  def filterFold(folds: List[Fold], srcParIdx: Int, tgtParIdx: Int): List[Fold] = {
    val srcPageIdx = getParPage(srcParIdx)
    val tgtPageIdx = getParPage(tgtParIdx)

    val f0 = folds.filter { f =>
      f.pages.exists(_.idx == srcPageIdx) &&
      f.pages.exists(_.idx == tgtPageIdx)
    }

    val f1 = f0.map { f =>
      val p1 = f.pages.dropWhile(_.idx <  srcPageIdx)
      val p2 = p1     .takeWhile(_.idx <= tgtPageIdx)
      f.copy(pages = p2)  // preserve `id`
    }

    val srcIsRight = getParColumn(srcParIdx) == 1
    val tgtIsLeft  = getParColumn(tgtParIdx) == 0

    val f2 = if (!srcIsRight) f1 else f1.filter { f =>
      f.pages match {
        case p1 :: p2 :: _ if p1.rotation == p2.rotation => true
        case _ => false
      }
    }

    val f3 = if (!tgtIsLeft) f2 else f2.filter { f =>
      f.pages.takeRight(2) match {
        case p1 :: p2 :: Nil if p1.rotation == p2.rotation => true
        case _ => false
      }
    }

//    val f4 = f3.map(_.normalize).distinct  // distinct has floating point prob
    val f4 = f3.map(_.normalize)
    distinctFolds(f4)
  }

  def distinctFolds(folds: List[Fold]): List[Fold] = {
    val f5 = folds.iterator.map(f => f.id -> f).toMap.toList
    val f6 = f5.sortBy { case (fId, _) => folds.indexWhere(_.id == fId) } .map(_._2)
    f6
  }

  def viewGNG(fold: Fold): Unit = {
    val gr = readGNG(fold)
//    val startIdx  = util.Random.nextInt(gr.nodes.size)
//    val stopIdx   = {
//      val i = util.Random.nextInt(gr.nodes.size - 1)
//      if (i >= startIdx) i + 1 else i
//    }
    val rnd       = new util.Random(108L)
    val left      = gr.nodes.iterator.zipWithIndex.filter(_._1.x < gr.surfaceWidthPx * 1/3).map(_._2).toIndexedSeq
    val right     = gr.nodes.iterator.zipWithIndex.filter(_._1.x > gr.surfaceWidthPx * 2/3).map(_._2).toIndexedSeq
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
          val sx    = w.toDouble / gr.surfaceWidthPx
          val sy    = h.toDouble / gr.surfaceHeightPx
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
        title = fold.rotationString
        contents = comp
        preferredSize = new Dimension(1600, 400)
        pack().centerOnScreen()
        open()
      }
    }
  }

  def mkFoldImage(info: Vec[ParFileInfo], fold: Fold, color: Boolean = false): BufferedImage = {
    val w   = math.round(ppmm * fold.surfaceMM.width ).toInt
    val h   = math.round(ppmm * fold.surfaceMM.height).toInt
    val img = new BufferedImage(w, h, if (color) BufferedImage.TYPE_INT_ARGB else BufferedImage.TYPE_BYTE_BINARY)
    val g   = img.createGraphics()
    val c0  = Color.white
    val c1  = Color.black

    def fillRectMM(r: Rectangle): Unit = {
      val rs = r.scale(ppmm)
      val xi = math.round(rs.x).toInt
      val yi = math.round(rs.y).toInt
      val wi = math.round(rs.x + rs.width ).toInt - xi
      val hi = math.round(rs.y + rs.height).toInt - yi
      g.fillRect(xi, yi, wi, hi)
    }

    g.setColor(c0)
    g.fillRect(0, 0, w, h)
    g.setColor(c1)
    fold.pages.foldLeft(Option.empty[Rectangle]) { case (rPred, foldPage) =>
      val r0 = foldPage.rectangle
      val r1 = r0.inset(PadParMM)
      val r2 = rPred.fold(r1)(_ union r1)
      val r3 = r0.border(PadParMM)
      val r4 = r3.intersect(r2)
      if (color) {
        val c2 = if (foldPage.isUpright) c1
          else if (foldPage.isCCW) Color.red
          else new Color(0, 0x80, 0)
        g.setColor(c2)
      }
      fillRectMM(r4)
      Some(r1)
    }
    g.setColor(c0)
    fold.pages.foreach { foldPage =>
      foldPage.par.foreach { rp0 =>
        fillRectMM(rp0)
      }
    }
    g.dispose()
    img
  }

  def testViewFolds(): Unit = {
    val info    = Catalog.readOrderedParInfo()
    val folds0  = mkFoldSeq(info, Lang.De)
    val maxRect = folds0.map(_.surfaceMM).reduce(_ union _).scale(ppmm)
    Swing.onEDT {
      var folds : List[Fold]    = folds0
      var fold  : Fold          = folds.head
      var img   : BufferedImage = mkFoldImage(info, fold, color = true)

      val ggImage = new Component {
        preferredSize = new Dimension(1600, 800)

        override protected def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          g.setColor(Color.gray)
          val w = peer.getWidth
          val h = peer.getHeight
          g.fillRect(0, 0, w, h)
          val sx = w.toDouble / maxRect.width
          val sy = h.toDouble / maxRect.height
          val scale = math.min(sx, sy)
          val wi = (img.getWidth  * scale).toInt
          val hi = (img.getHeight * scale).toInt
          val xi = (w - wi)/2
          val yi = (h - hi)/2
          g.drawImage(img, xi, yi, wi, hi, peer)
          g.setColor(Color.yellow)
          g.drawString(fold.rotationString, 8, 24)
        }
      }

      val ggSelect = new ComboBox(folds.indices) {
        listenTo(selection)
     }

      def updateImage(): Unit = {
        img.flush()
        val id  = folds0(ggSelect.selection.item).id
        fold    = folds.find(f => id.contains(f.id)).get
        img     = mkFoldImage(info, fold, color = true)
        ggImage.repaint()
      }

      ggSelect.reactions += {
        case SelectionChanged(_) =>
          updateImage()
      }

      val mSrc = new SpinnerNumberModel(-1, -1, 17, 1)
      val mTgt = new SpinnerNumberModel(-1, -1, 17, 1)

      def setFilter(): Unit = {
        val srcParIdx = mSrc.getNumber.intValue
        val tgtParIdx = mTgt.getNumber.intValue
        if (srcParIdx >= 0 && tgtParIdx > srcParIdx) {
          folds = filterFold(folds0, srcParIdx = srcParIdx, tgtParIdx = tgtParIdx)
          val filtered = folds.map { fF =>
            val idF = fF.id
            val i = folds0.indexWhere(_.id.contains(idF))
            i
          }
          ggSelect.items = filtered
        } else {
          folds = folds0
          ggSelect.items = folds.indices
        }
        updateImage()
      }

      val ggSrc = new Spinner(mSrc) {
        reactions += {
          case ValueChanged(_) => setFilter()
        }
      }

      val ggTgt = new Spinner(mTgt) {
        reactions += {
          case ValueChanged(_) => setFilter()
        }
      }

      val pEast = new GridPanel(3, 2) {
        contents += new Label("Fold Idx:")
        contents += ggSelect
        contents += new Label("Src Par Idx:")
        contents += ggSrc
        contents += new Label("Tgt Par Idx:")
        contents += ggTgt
      }

      new Frame {
        title     = "Folds"
        contents  = new BorderPanel {
          add(ggImage , BorderPanel.Position.Center)
          add(pEast   , BorderPanel.Position.East  )
        }
        pack().centerOnScreen()
        open()
      }
    }
  }

  def runGNG(info: Vec[ParFileInfo], fold: Fold, fOutGNG: File): Unit = {
//    val info          = Catalog.readOrderedParInfo()
//    val SurfaceWidthPx      = PageWidthPx  * 6
//    val SurfaceHeightPx     = PageHeightPx * 1
//    println(s"surface = $SurfaceWidthPx x $SurfaceHeightPx")
//    val folds         = mkFoldSeq  (info)
//    val fold          = folds(2)
    val img           = mkFoldImage(info, fold)

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

    val SurfaceWidthPx  = img.getWidth
    val SurfaceHeightPx = img.getHeight

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
