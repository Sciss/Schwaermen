/*
 *  ExplorePaths.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen

import java.awt.{Color, Font}

import de.sciss.desktop.Desktop
import de.sciss.kollflitz
import de.sciss.numbers
import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import org.jfree.chart.ChartPanel
import org.jfree.chart.plot.{CategoryPlot, XYPlot}
import org.jfree.chart.renderer.xy.{StandardXYBarPainter, XYBarRenderer, XYStepAreaRenderer}

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.swing.{Component, MainFrame, Swing}
import scala.util.Random
import scalax.chart.{Chart, XYChart}

object ExplorePaths {
  def CALC_LENGTH = true
  def WEIGHT_POW  = 1.0 // no effect on MST!

  def main(args: Array[String]): Unit = {
    val edges     = ShowSimilarities.loadAndSortGraph(1::3::Nil, weightPow = WEIGHT_POW, dropAmt = 0.1)
    val map       = mkEdgeMap(edges)
    val vertices  = map.keys.toIndexedSeq

    if (CALC_LENGTH) {
      val lengths = vertices.combinations(2).map {
        case Seq(v1, v2) =>
          val res = calcPath(v1, v2, map).size
          if (res == 0)
            println(s"WARNING: For ${v1.quote} -- ${v2.quote}, no path was found!")
          if (res == 1)
            println(s"WARNING: For ${v1.quote} -- ${v2.quote}, path is only one element!")
          math.max(1, res - 1)
      }   .toVector
      val numComb   = lengths.size
      val totalLen  = lengths.sum.toDouble
      import kollflitz.Ops._

      println(s"Number of combinations: $numComb; total length: $totalLen")
      val mean      = totalLen / numComb
      val median    = lengths.sortedT.median
      val h         = lengths.counted
      val hk        = h.keys
      val minCount  = hk.min
      val maxCount  = hk.max
      println(f"For weightPow = $WEIGHT_POW%g, mean path lengths = $mean%g, median = $median, min = $minCount, max = $maxCount")
      val accum     = Vector.tabulate(maxCount - minCount + 1)(i => h(i + minCount))

      Swing.onEDT {
        val chart = mkHistogramChart(accum, xMin = minCount, xMax = maxCount,
          title = f"DFS Path Lengths Histogram (weight pow = $WEIGHT_POW%g)")
        val pj = new ChartPanel(chart.peer, false)
        val p  = Component.wrap(pj)
        val f  = new MainFrame {
          contents = p
          //        new pdflitz.SaveAction(List(p)).setupMenu(this)
          pack().centerOnScreen()
        }
        f.open()
      }

    } else {
      val verticesR = Random.shuffle(vertices)
      verticesR.combinations(2).take(10).foreach {
        case Seq(v1, v2) =>
          println(s"source = ${v1.quote}, sink = ${v2.quote}")
          val path      = calcPath(v1, v2, map)
          println(path.map(_.quote).mkString("Path:\n  ", "\n  ", ""))
      }
    }
  }

  def mkHistogramChart(histo: Vector[Int], xMin: Int, xMax: Int, title: String): XYChart = {
    import numbers.Implicits._

    import scalax.chart.module.Charting._
    val data: Vector[(Int, Int)] = histo.zipWithIndex.map { case (num, i) =>
      (i + 0.5).linlin(0, histo.length, xMin, xMax).toInt -> num
    } (breakOut)
    val dataCol = data.toXYSeriesCollection(title)
    val chart   = XYLineChart(dataCol)
    chart.title = title
//    chart.legend = false
    mkNiceChart(chart)
    val plot    = chart.plot
    val renderer = new XYStepAreaRenderer()
    plot.setRenderer(renderer)
    renderer.setSeriesPaint(0, Color.darkGray)
    chart
  }

  private val defaultFontFace =
    if      (Desktop.isLinux  ) "Liberation Sans"
    else if (Desktop.isWindows) "Arial"
    else                        "Helvetica"

  def mkNiceChart(chart: Chart): Unit = {
    val plot = chart.plot

    val (xAxis, yAxis) = plot match {  // shitty Plot / Renderer interfaces do not have common super types
      case p: XYPlot       =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
      case p: CategoryPlot =>
        p.setBackgroundPaint           (Color.white    )
        p.setDomainGridlinePaint       (Color.lightGray)
        p.setRangeGridlinePaint        (Color.lightGray)
        p.getRenderer.setSeriesPaint(0, Color.darkGray )
        // undo the crappy "3D" look
        p.getRenderer match {
          case r: XYBarRenderer => r.setBarPainter(new StandardXYBarPainter())
          case _ =>
        }
        (p.getDomainAxis, p.getRangeAxis)
    }

    val fnt1          = new Font(defaultFontFace, Font.BOLD , 14)
    val fnt2          = new Font(defaultFontFace, Font.PLAIN, 12)
    xAxis.setLabelFont(fnt1)
    xAxis.setTickLabelFont(fnt2)
    yAxis.setLabelFont(fnt1)
    yAxis.setTickLabelFont(fnt2)
  }

  type EdgeMap = Map[Vertex, Set[SimEdge]]

  def mkEdgeMap[A](edges: List[Edge[A]]): Map[A, Set[Edge[A]]] = {
    @tailrec
    def loop(rem: List[Edge[A]], res: Map[A, Set[Edge[A]]]): Map[A, Set[Edge[A]]] =
      rem match {
        case edge :: tail =>
          val oldS = res.getOrElse(edge.start, Set.empty)
          val oldE = res.getOrElse(edge.end  , Set.empty)
          val newS = oldS + edge
          val newE = oldE + edge
          val newM = res + (edge.start -> newS) + (edge.end -> newE)
          loop(tail, newM)

        case _ => res
      }

    loop(edges, Map.empty)
  }

  /** Uses depth-first-search. Requires that the graph has no cycles.
    * Returns `Nil` if no path is found.
    */
  def calcPath[A](source: A, sink: A, map: Map[A, Set[Edge[A]]]): List[A] = {
    @tailrec
    def loop(back: List[(A, Set[Edge[A]])], rem: Map[A, Set[Edge[A]]]): List[A] =
      back match {
        case (v, edges) :: tail =>
          if (v == sink) back.reverseIterator.map(_._1).toList
          else if (edges.nonEmpty) {
            val edge    = edges.head
            val vTail   = edges - edge
            val target  = if (edge.start != v) edge.start else edge.end
            val tTail   = rem(target) - edge
            val remNew  = if (tTail.isEmpty) rem - target else rem + (target -> tTail)
            val backNew = (target -> tTail) :: (v -> vTail) :: tail
            loop(backNew, remNew)

          } else {
            loop(tail, rem)
          }

        case _ => Nil
      }

    map.get(source) match {
      case Some(sEdges) => loop((source -> sEdges) :: Nil, map)
      case None         => Nil
    }
  }
}
