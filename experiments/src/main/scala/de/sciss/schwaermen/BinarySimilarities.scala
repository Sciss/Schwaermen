///*
// *  BinarySimilarities.scala
// *  (Schwaermen)
// *
// *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
// *
// *  This software is published under the GNU General Public License v2+
// *
// *
// *  For further information, please contact Hanns Holger Rutz at
// *  contact@sciss.de
// */
//
//package de.sciss.schwaermen
//
//import de.sciss.file._
//import de.sciss.kollflitz.Vec
//import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
//
//import scala.collection.breakOut
//
//object BinarySimilarities {
//  def triangularIndex(numVertices: Int, start: Short, end: Short): Int = {
//    val numM              = numVertices - start
//    val numEdgesComplete  = (numVertices * (numVertices  - 1)) / 2
//    numEdgesComplete - (numM * (numM - 1)) / 2 - start + (end - 1)
//  }
//
//  /** Converts the output from `BuildSimilarities` into a reduced and
//    * more regular format easy to read for `PathFinder`.
//    * Among other things, produces _all_ edges
//    */
//  def convert(textIndices: Seq[Int], fOut: File): Unit = {
//    val edges: List[SimEdge] = ShowSimilarities.loadGraph(textIndices = textIndices)
//    val edgeMap = edges.groupBy(_.start.index).map {
//      case (start, targets) => start -> targets.groupBy(_.end)
//    }
//    val allVertices: Vec[Vertex] = {
//      val tmp: Vec[Vertex] = edges.flatMap(e => e.start :: e.end :: Nil)(breakOut)
//      tmp.distinct.sorted
//    }
//    val numVertices = allVertices.size
//    val edgeMapReg = (0 until numVertices).combinations(2).foreach {
//      case (start, end) =>
//        val v1    = allVertices(start)
//        val v2    = allVertices(end )
//        val edge  =   ...
//    }
//  }
//}
