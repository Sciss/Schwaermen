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

import java.io.FileOutputStream

import de.sciss.file._
import de.sciss.kollflitz.Vec

object Catalog {
  def main(args: Array[String]): Unit = {
    run()
  }

  private def mkText(in: String): String = {
    val s0 = in.stripMargin
    val s1 = s0.replace('\n', ' ')
    val s2 = s1.replace("  ", " ")
    s2.trim
  }

  val parDe: Vec[String] = Vector(
    """Die Gassen sind eng und mit starkem Gefälle hinunter zum Wasser. Ich gehe noch einmal zurück.
      |Alles ist von einer starken Vertikalität geprägt. Ich ziehe mich an einer Wand hoch, an Seilen
      |und Griffmöglichkeiten. Ist das der einzige Weg oder nur eine Abkürzung? Missbilligung bei den
      |Umstehenden? Eine Ecke des Raumes ist aus Glas, vom Boden zur Decke, ein riesiges Fenster nach
      |draußen, orangefarbenes Licht fällt herein. Jemand ruft mich an, er und seine Partnerin hätten
      |Schwierigkeiten gehabt, die Installation in der Ausstellung zu verstehen. Dort sind
      |vibrierende, sich mit hoher Frequenz und geringer Amplitude schüttelnde Dinge montiert auf
      |einem Feld oder Tisch, von der Größe kleiner Äpfel.
      |""",
    """Es handelt sich um ein Experiment. Zwei menschliche Arme werden zusammengelegt, nebeneinander,
      |dicht beieinander. Sie werden gequetscht oder seziert, geschnitten, dann vereint auf einem
      |Objektträger oder Glas. Ein Exemplar wird hergestellt, der \emph{Ausschnitt} des doppelten
      |Arms wird zu einem Objekt. Was passiert mit dem Leben? Das Leben war ja vormalig separat in
      |jedem der Arme. Inwieweit vereint sich das Leben in den zusammengequetschten Armen, wo es doch
      |auf zwei unterschiedliche Linien zurückverfolgt werden kann? Tim Ingold erinnert an Deleuze's
      |Verwendung der Haecceitas oder des \emph{Bündels} von Linien zur Bezeichnung des lebendigen
      |Organismus. Die Traurigkeit der Unwiederholbarkeit: In einer Erfahrung vereint, die jedoch nie
      |stattfand, ausser in der Erinnerung.
      |"""
  ).map(mkText)

  val latexTemplate: String =
    """@documentclass[10pt,twoside,twocolumn]{article}
      |@usepackage[paperheight=20cm,paperwidth=20cm,top=25mm,bottom=25mm,right=25mm,left=25mm,heightrounded]{geometry}
      |@usepackage[ngerman]{babel}
      |@usepackage{Alegreya}
      |@usepackage[T1]{fontenc}
      |@usepackage[utf8]{inputenc}
      |
      |@begin{document}
      |@pagestyle{empty}
      |@fontsize{8.5pt}{10.2pt}\selectfont
      |@noindent
      |%s
      |@end{document}""".stripMargin.replace('@', '\\')

  def writeText(s: String, f: File): Unit = {
    val fos = new FileOutputStream(f)
    try {
      fos.write(s.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
  }

  // these you need to have installed
  val pdflatex: String = "pdflatex"
  val inkscape: String = "inkscape"

  def run(): Unit = {
    import sys.process._

    val text  = parDe.last
    val latex = latexTemplate.format(text)
    val dir   = file("/") / "data" / "temp" / "latex"
    require (dir.isDirectory)
    val fOutTex = dir / "test.tex"
    val fOutPDF = fOutTex.replaceExt("pdf")
    writeText(latex, fOutPDF)

    val cmdPDF = Seq(pdflatex, "-interaction=batchmode", fOutTex.path)
    val resPDF = Process(cmdPDF, dir).!
    require (resPDF == 0, s"pdflatex failed with code $resPDF")

    val fOutSVG = fOutPDF.replaceExt("svg")
    val cmdSVG = Seq(inkscape, "-l", fOutSVG.path, fOutPDF.path)
    val resSVG = Process(cmdSVG, dir).!
    require (resSVG == 0, s"inkscape failed with code $resSVG")

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
    val group   : xml.NodeSeq = svgDoc \ "g"
    val textSeq : xml.NodeSeq = group  \ "text"

    println(s"textSeq.size = ${textSeq.size}")  // 5 : regular, italics, regular, italics, regular
  }
}