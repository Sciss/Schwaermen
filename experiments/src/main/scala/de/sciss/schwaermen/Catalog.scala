/*
 *  Catalog.scala
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

import java.awt.Font
import java.awt.geom.AffineTransform
import java.io.{FileInputStream, FileOutputStream}
import java.nio.channels.Channels

import de.sciss.file._
import de.sciss.numbers

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object Catalog {
  def main(args: Array[String]): Unit = {
//    val t0 = Transform(1.2, 3.4, 5.6, 7.8, 9.10, 11.12)
//    val t1 = Transform.fromAwt(t0.toAwt)
//    assert(t0 == t1)

    if (!fLinesOut.isFile || !fOutCat.isFile || !parFile(1).isFile) {
      preparePar()
    } else {
      println("(Skipping preparePar)")
    }

    if (!fOutArr.isFile) {
      renderPages()
    } else {
      println("(Skipping renderPages)")
    }
  }

  val dir       : File = file("/") / "data" / "projects" / "Schwaermen" / "catalog" / "hhr"
  val dirTmp    : File = file("/") / "data" / "temp" / "latex"
  val fOutCat   : File = dir / "par_cat.pdf"
  val fOutArr   : File = dir / "par_arr.pdf"
  val fLinesOut : File = dir / "par_lines.txt"

  val PaperWidthMM  : Int = 200
  val PaperHeightMM : Int = 200
  val MarginTopMM   : Int =  25
  val MarginBotMM   : Int =  25
  val MarginLeftMM  : Int =  25
  val MarginRightMM : Int =  25

//  val MarginParMM   : Int =  25
//  val WidthParMM    : Int = (200 - 25 - 25)/2
//  val HeightParMM   : Int = WidthParMM
//
//  val WidthParBMM   : Int = WidthParMM  + MarginParMM + MarginParMM
//  val HeightParBMM  : Int = HeightParMM + MarginParMM + MarginParMM

  val ColumnSepMM   : Int     = 25
  val LineSpacingPt : Double  = 10.2
  val FontSizePt    : Double  = 8.5
  val PointsPerMM   : Double  = 72 / 25.4   // 1 pt = 1/72 inch

  val WidthParMM    : Double  = (PaperWidthMM - (MarginLeftMM + MarginRightMM + ColumnSepMM))/2.0

  def HeightParMM(numLines: Int): Double = {
    val lnMM = LineSpacingPt / PointsPerMM
    lnMM * numLines
  }

  def readParInfo(): Vec[ParFileInfo] = {
    val s          = readText(fLinesOut)
    val numLinesIt = s.split("\n").iterator.map(_.trim).filter(_.nonEmpty).map(_.toInt)
    numLinesIt.zipWithIndex.map { case (numLines, i) =>
      ParFileInfo(id = i + 1, numLines = numLines)
    } .toIndexedSeq
  }

  // N.B.: horizontal line filling is broken if we use single column
  def latexParTemplate(text: String): String = stripTemplate(
    s"""@documentclass[10pt,twocolumn]{article}
       |@usepackage[paperheight=${PaperHeightMM}mm,paperwidth=${PaperWidthMM}mm,top=${MarginTopMM}mm,bottom=${MarginBotMM}mm,right=${MarginRightMM}mm,left=${MarginLeftMM}mm,heightrounded]{geometry}
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

  private def stripTemplate(s: String): String = s.stripMargin.replace('@', '\\')

  def renderPages(): Unit = {
    val pre = stripTemplate(s"""@documentclass[10pt]{article}
       |@usepackage[paperheight=${PaperHeightMM}mm,paperwidth=${PaperWidthMM}mm,top=0mm,bottom=0mm,left=0mm,right=0mm]{geometry}
       |@usepackage{tikz}
       |@usepackage{graphicx}
       |@usetikzlibrary{calc}
       |@begin{document}
       |@pagestyle{empty}
       |""")

    val post = stripTemplate(
      """@end{document}
        |""")

    def mkGraphics(f: ParFileInfo, heightMM: Double, leftMM: Double, topMM: Double): String = {
      val trimLeft    = MarginLeftMM
      val trimTop     = MarginTopMM
      val trimBottom  = PaperHeightMM - MarginBotMM - heightMM
      val trimRight   = PaperWidthMM - MarginRightMM - WidthParMM

//      @fbox{@includegraphics[scale=1,trim=${trimLeft}mm ${trimBottom}mm ${trimRight}mm ${trimTop}mm]{${parFile(f.id)}}}

      stripTemplate(
        s"""  @node[anchor=north west,inner sep=0] at ($$(current page.north west)+(${leftMM}mm,${-topMM}mm)$$) {
           |    @includegraphics[scale=1,trim=${trimLeft}mm ${trimBottom}mm ${trimRight}mm ${trimTop}mm]{${parFile(f.id)}}
           |  };
           |"""
      )
    }

    def mkPage(f1: ParFileInfo, f2: ParFileInfo, f3: ParFileInfo): String = {
      val h1 = HeightParMM(f1.numLines)
      val h2 = HeightParMM(f2.numLines)
      val h3 = HeightParMM(f3.numLines)
      val x1 = MarginLeftMM
      val x2 = PaperWidthMM - MarginRightMM - WidthParMM
      val x3 = x1
      val y1 = MarginTopMM
      val y2 = (PaperHeightMM - h2)/2 // XXX TODO
      val y3 = PaperHeightMM - MarginBotMM - h3

      stripTemplate(
        s"""@begin{tikzpicture}[remember picture,overlay]
           |${mkGraphics(f1, h1, x1, y1)}
           |${mkGraphics(f2, h2, x2, y2)}
           |${mkGraphics(f3, h3, x3, y3)}
           |@end{tikzpicture}
           |"""
        )
    }

    val info0     = readParInfo()
    val info      = CatalogTexts.parOrder.map(id => info0(id - 1))
    val pageInfo  = info.grouped(3)
    val pages     = pageInfo.map { case Seq(f1, f2, f3) => mkPage(f1, f2, f3) }
    val text      = pages.mkString(pre, "\\newpage\n", post)

    val fArrTex   = dirTmp / s"${fOutArr.base}.tex"
    writeText(text, fArrTex)
    val fArrPDF   = fArrTex.replaceExt("pdf")
    val argPDF    = Seq("-interaction=batchmode", fArrTex.path)
    exec(pdflatex, dirTmp, argPDF)
    exec("cp", dirTmp, fArrPDF.path :: fOutArr.path :: Nil)
  }

  def writeText(s: String, f: File): Unit = {
    val fos = new FileOutputStream(f)
    try {
      fos.write(s.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
  }

  def readText(f: File): String = {
    val fis = new FileInputStream(f)
    try {
      val arr = new Array[Byte](fis.available())
      fis.read(arr)
      new String(arr, "UTF-8")
    } finally {
      fis.close()
    }
  }

  // these you need to have installed
  val pdflatex: String = "pdflatex"
  val inkscape: String = "inkscape"
  val pdftk   : String = "pdftk"

  case class Style(font: Font)

  object Point2D {
    def fromAwt(in: java.awt.geom.Point2D): Point2D =
      apply(in.getX, in.getY)
  }
  case class Point2D(x: Double, y: Double) {
    def toAwt: java.awt.geom.Point2D = new java.awt.geom.Point2D.Double(x, y)
  }

  object Transform {
    def fromAwt(in: AffineTransform): Transform =
      apply(in.getScaleX, in.getShearY, in.getShearX, in.getScaleY, in.getTranslateX, in.getTranslateY)
  }
  case class Transform(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double) {
    private[this] lazy val at = new AffineTransform(a, b, c, d, e, f)

    def toAwt: AffineTransform = new AffineTransform(at)

    def apply(pt: Point2D): Point2D =
      Point2D.fromAwt(at.transform(pt.toAwt, null))
  }

  case class TSpan(id: String, y: Double, x: ISeq[Double], text: String, node: xml.Elem) {
    def start: Point2D = Point2D(x.head, y)

    def size: Int = x.size

    def apply(idx: Int): TSpan = slice(idx, idx + 1)

    def slice(from: Int, until: Int): TSpan = {
      val id1 = s"$id-$from-$until"
      val x1  = x.slice(from, until)
      val t1  = text.substring(from, until)
      val n1  = setId(node, id1)
      val n2  = setAttr(n1, "x", x1.map(_.toFloat).mkString(" "))
      val n3  = n2.copy(child = new xml.Text(t1) :: Nil)
      copy(id = id1, y = y, x = x1, text = t1, node = n3)
    }
  }

  def setAttr(in: xml.Elem, key: String, value: String): xml.Elem = {
    val a = xml.Attribute(pre = null, key = key, value = value, next = xml.Null)
    in % a
  }

  def setId(in: xml.Elem, id: String): xml.Elem =
    setAttr(in, key = "id", value = id)

  case class Text(id: String, style: Style, transform: Transform, children: ISeq[TSpan], node: xml.Elem) {
    def setTransform(t: Transform): Text = {
      val ts = s"matrix(${t.a.toFloat},${t.b.toFloat},${t.c.toFloat},${t.d.toFloat},${t.e.toFloat},${t.f.toFloat})"
      val n1 = setAttr(node, "transform", ts)
      copy(node = n1)
    }

    def splitAt(idx: Int): (Text, Text) = {
      val (c1, c2) = node.child.splitAt(idx)
      val (n1, n2) = (node.copy(child = c1), node.copy(child = c2))
      val (c3, c4) = children.splitAt(idx)
      (copy(children = c3, node = n1), copy(children = c4, node = n2))
    }

    def size: Int = children.size

    def apply(idx: Int): Text = slice(idx, idx + 1)

    def slice(from: Int, until: Int): Text = {
      val id1 = s"$id-$from-$until"
      val c1  = node.child.slice(from, until)
      val n1  = node.copy(child = c1)
      val n2  = setId(n1, id1)
      val c3  = children.slice(from, until)
      copy(id = id1, children = c3, node = n2)
    }

    def chop(idx: Int): ISeq[Text] = {
      val t1  = apply(idx)
      val ts  = t1.children.head
      ISeq.tabulate(ts.size) { i =>
        val id2 = s"${t1.id}-ch$i"
        val ts1 = ts(i)
        val n1  = node.copy(child = ts1.node)
        val n2  = setId(n1, id2)
        t1.copy(id = id2, children = ts1 :: Nil, node = n2)
      }
    }
  }

  def parseStyle(n: xml.NodeSeq): Style = {
    val m: Map[String, String] = n.text.split(';').iterator.flatMap(_.split(':') match {
      case Array(key, value) => Some((key, value))
      case _ => None
    }) .toMap

//    (key, value) match {
//      case "font-style:italic;font-variant:normal;font-size:8.46819973px;font-family:Alegreya;"
//    }

    val fntSize   = m("font-size")
    val fntFamily = m("font-family")
    val fntSizeD  = {
      // XXX TODO -- somehow inkscape treats 1 px = 1 pt here?
      require (fntSize.endsWith("px"))
      fntSize.substring(0, fntSize.length - 2).toFloat
    }
    val name      = if (m.get("font-style").contains("italic")) s"$fntFamily Italic" else fntFamily
    val font      = new Font(name, Font.PLAIN, 1).deriveFont(fntSizeD)

    Style(font) // fontOpt.map(Style).getOrElse(sys.error(s"Could not parse style from $n and $m"))
  }

  def parseTransform(n: xml.NodeSeq): Transform = {
    val t = n.text.trim
    if (t.startsWith("matrix(") && t.endsWith(")")) {
      val in = t.substring(t.indexOf('(') + 1, t.lastIndexOf(')'))
      in.split(',').map(_.toDouble) match {
        case Array(a, b, c, d, e, f) => Transform(a, b, c, d, e, f)
        case _ => sys.error("no")
      }

    } else sys.error(s"Could not parse transform $n")
  }

  def parseTSpan(n: xml.Node): TSpan = n match {
    case e0: xml.Elem if e0.label == "tspan" =>
      val id = (n \ "@id").text
      val y = (n \ "@y").text.trim.toDouble
      val x = (n \ "@x").text.split(' ').iterator.map(_.trim.toDouble).toList
      val text0 = n.text.trim
      val text  = CatalogTexts.debugWords(text0)
      val e = if (text == text0) e0 else {
        e0.copy(child = xml.Text(text))
      }
      TSpan(id = id, y = y, x = x, text = text, node = e)

    case _ => sys.error(s"Not a tspan: $n")
  }

  def parseText(n: xml.Node): Option[Text] = n match {
    case e: xml.Elem if e.label == "text" =>
      val id        = (e \ "@id").text
      val style     = parseStyle     (e \ "@style")
      val transform = parseTransform (e \ "@transform")
      val children  = (e \ "tspan").map(parseTSpan)
      val e1        = e.copy(child = children.map(_.node))
      val res       = Text(id = id, style = style, transform = transform, children = children, node = e1)
      Some(res)

    case _ => None // sys.error(s"Could not parse text $n")
  }

  def preparePar(): Unit = {
    require (dir.isDirectory, s"Not a directory: $dir")
    val resAll = CatalogTexts.parDe.zipWithIndex.map { case (text, i) =>
      val textId = i + 1
      println(s"RENDERING $textId")
      run(text = text, textId = textId)
    }
    val numLinesAll = resAll.map(_.numLines)
    val numLinesTxt = numLinesAll.mkString("", "\n", "\n")
    writeText(numLinesTxt, fLinesOut)

    val fOutAll     = resAll.map(_.id)
    val argsCat     = fOutAll.map(parFile(_).path) :+ "cat" :+ "output" :+ fOutCat.path
    exec(pdftk, dir, argsCat)
  }

  case class ParFileInfo(id: Int, numLines: Int)

  def parFile(id: Int): File = dir / s"par_$id.pdf"

  def run(text: String, textId: Int): ParFileInfo = {
    val latex = latexParTemplate(text)
    require (dirTmp.isDirectory, s"Not a directory: $dirTmp")
    val fOutTex = dirTmp / s"par_temp_$textId.tex"
    val fOutPDF = fOutTex.replaceExt("pdf")
    writeText(latex, fOutTex)

    val argPDF = Seq("-interaction=batchmode", fOutTex.path)
    exec(pdflatex, dirTmp, argPDF)

    val fOutSVG = fOutPDF.replaceExt("svg")
    val argSVG = Seq("-l", fOutSVG.path, fOutPDF.path)
    exec(inkscape, dirTmp, argSVG)

    /*

    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <svg viewBox="0 0 755.90533 755.90533" height="755.90533" width="755.90533">
      <g transform="matrix(1.3333333,0,0,-1.3333333,0,755.90533)">
        <text id="text22"
          style="font-variant:normal;font-weight:normal;font-size:8.46819973px;font-family:Alegreya;-inkscape-font-specification:Alegreya-Regular;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
          transform="matrix(1,0,0,-1,70.866,486.1)">

          <tspan id="tspan12"
            y="0"
            x="0 5.0047064 11.118747 15.640765 19.586946 24.346075 28.893497 32.687252 34.753494 40.249355 43.653572 46.041603 49.632122 56.863964 61.445259 71.200623 74.994377 77.382408 84.842896 89.847603 93.869995 98.341209 102.13496 105.37828 107.76631 114.81186 118.60561 123.36474 126.15924 130.88451 135.69444 141.66452 145.45827 150.55612 157.60167 161.39542 166.15456 169.55876 173.14929 177.67131 179.73755 182.12558 185.71609 190.23811 196.73322 201.62784 204.87117"
            >EshandeltsichumeinExperiment.ZweimenschlicheAr-</tspan>

          ...
        </text>
        <text id="text26"
          style="font-style:italic;font-variant:normal;font-size:8.46819973px;font-family:Alegreya;-inkscape-font-specification:Alegreya-Italic;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
          transform="matrix(1,0,0,-1,124.166,445.453)">

          ...
        </text>
      </g>
    </svg>

     */

    val svgDoc  : xml.Elem    = xml.XML.loadFile(fOutSVG)
    val group   : xml.Elem    = (svgDoc \ "g").head.asInstanceOf[xml.Elem]
//    val textSeq : xml.NodeSeq = group  \ "text"

//    println(s"textSeq.size = ${textSeq.size}")  // 5 : regular, italics, regular, italics, regular

    val (groupOut, numLines) = {
      val tt0: Seq[Text] = group.child.flatMap(parseText)
      val _numLines = tt0.iterator.flatMap { t =>
        val y0 = t.transform.f
        t.children.map(_.start.y + y0)
      }.toSet.size

//      println(s"numLines = $numLines")

      val (th, tt1) = {
        val ttx             = tt0.head
        val y0              = ttx.transform(ttx.children.head.start).y
        val (t1, t2)        = tt0.span(t => t.children.forall(c => t.transform(c.start).y == y0))
        val t2h +: t2t      = t2
        val i               = t2h.children.indexWhere(c => t2h.transform(c.start).y != y0)
        val (t3, t4)        = t2h.splitAt(i)
        val _th : Seq[Text] = t1 :+ t3
        val _tt1: Seq[Text] = t4 +: t2t
        (_th, _tt1)
      }

      val (tl, tm) = {
        val ttx             = tt1.last
        val y0              = ttx.transform(ttx.children.last.start).y
        val (t1r, t2r)      = tt1.reverse.span(t => t.children.forall(c => t.transform(c.start).y == y0))
        val t1              = t1r.reverse
        val t2              = t2r.reverse
        val t2i :+ t2l      = t2
        val i               = t2l.children.lastIndexWhere(c => t2l.transform(c.start).y != y0) + 1
        val (t4, t3)        = t2l.splitAt(i)
        val _tl: Seq[Text]  = t1  :+ t3
        val _tm: Seq[Text]  = t2i :+ t4
        (_tl, _tm)
      }

      def fade(in: Seq[Text], dir: Boolean): Seq[Text] =
        in.flatMap { t =>
          val flat = t.children.indices.flatMap { i =>
            t.chop(i)
          }
          val n = flat.size
          flat.zipWithIndex.map { case (_t, ti) =>
            val t0  = _t.transform
            val at0 = t0.toAwt
            import numbers.Implicits._
            val sy  =
              if (dir) (ti + 1).linlin(0, n, 0.0, 1.0)
              else      ti     .linlin(0, n, 1.0, 0.0)
            val at1 = AffineTransform.getScaleInstance(1.0, sy)
            // we have to compensate for the y coordinate inside the tspan
            val comp0 = _t.children.head.y * (1 - sy)
            val comp = if (dir) comp0 else comp0 - 4.0 * (1 - sy)
            at0.translate(0, comp)
            at0.concatenate(at1)
            val t1  = Transform.fromAwt(at0)
//            val t1 = t0.copy(d = t0.d * -sy, f = 733 - t0.f) // - t0.f * sy / 1.33333)
            _t.setTransform(t1)
          }
        }

      val thI = fade(th, dir = true )
      val tlI = fade(tl, dir = false)

      group.copy(child = (thI ++ tm ++ tlI).map(_.node)) -> _numLines
    }

    val svgDocCOut: Seq[xml.Node] = svgDoc.child.map {
      case `group` =>
//        println("GROUP-OUT")
        groupOut
      case x =>
//        println(s"OTHER: $x")
        x
    }
    val svgDocOut = svgDoc.copy(child = svgDocCOut)

    val fOutSVG2 = fOutSVG.parent / s"${fOutSVG.base}-out.svg"
    // XXX TODO --- pretty printing fails because it puts newlines before tspan text
    writePretty(svgDocOut, fOutSVG2, pretty = false)
//    val writer = new FileWriter(fOutSVG2)
//    try {
//      xml.XML.write(writer, node = svgDocOut, enc = "", xmlDecl = false, doctype = null)
//    } finally {
//      writer.close()
//    }

//    val fOutPDF2 = fOutSVG2.replaceExt("pdf")
    val fOutPDF2 = parFile(textId)
    val argsSVG2 = Seq("--export-pdf", fOutPDF2.path, fOutSVG2.path)
    exec(inkscape, dirTmp, argsSVG2)

    ParFileInfo(id = textId, numLines = numLines)
  }

  def exec(program: String, dir: File, args: Seq[String]): Unit = {
    import sys.process._
    val cmd = program +: args
    val res = Process(cmd, dir).!
    require (res == 0, s"$program, failed with code $res")
  }

  def writePretty(node: xml.Node, f: File, pretty: Boolean = true): Unit = {
    val pp      = new xml.PrettyPrinter(80, 2)
//    {
//      override protected def traverse(node: xml.Node, pScope: xml.NamespaceBinding, ind: Int): Unit =
//        node match {
//          case xml.Text(s) if s.trim() != "" => super.traverse(node, pScope, 0)
//          case _ => super.traverse(node, pScope, ind)
//        }
//    }
    val fos     = new FileOutputStream(f)
    val writer  = Channels.newWriter(fos.getChannel, "UTF-8")

    try {
      writer.write("<?xml version='1.0' encoding='UTF-8'?>\n")
      val s = if (pretty) pp.format(node) else node.toString()
      writer.write(s)
    } finally {
      writer.close()
    }
  }
}