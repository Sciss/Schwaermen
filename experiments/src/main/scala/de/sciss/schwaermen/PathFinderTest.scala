/*
 *  PathFinderTest.scala
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

import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import de.sciss.schwaermen.ExplorePaths.mkEdgeMap

import scala.util.Random

object PathFinderTest {
  def main(args: Array[String]): Unit = {
    run1()
  }

  def run2(): Unit = {
    implicit val rnd: Random = new Random(3L)
    val vertices  = 'A' to 'Z'
    val allEdges  = vertices.combinations(2).map {
      case Seq(a, b) => Edge(a, b)(rnd.nextInt(26))
    } .toList

    println(allEdges.mkString("\nAll edges:\n", "\n", ""))

    val mst1 = MSTKruskal[Char, Edge[Char]](allEdges)
//    println(mst1.mkString("\nOld:\n", "\n", ""))
    val edgeMap = mkEdgeMap(mst1)
    val resOld = ExplorePaths.calcPath(vertices.head, vertices.last, edgeMap)
    println(resOld.mkString("\nOld:\n", "\n", ""))

    val numVertices     = vertices.size
    val allEdgesSorted  = allEdges.sortBy(_.weight).map { e =>
      val v1i   = vertices.indexOf(e.start)
      val v2i   = vertices.indexOf(e.end  )
      val start = if (v1i < v2i) v1i else v2i
      val end   = if (v1i < v2i) v2i else v1i
//      println(s"$start -> $end")
      (start << 16) | end
    }   .toArray
    val finder = new PathFinder(numVertices = numVertices, allEdgesSorted = allEdgesSorted, maxPathLen = numVertices)
    val res0  = finder.findPath(0.toShort, (numVertices - 1).toShort)
    val res   = res0.map(vertices(_))
    println(res.mkString("\nNew:\n", "\n", ""))
  }

  def run0(): Unit = {
    // cf. http://i.imgur.com/yf8K1AK.png
    // but we have other MST

    val vertices  = "ABCDEFGH".toVector
    val allEdges  = List(
      Edge('A', 'B')(11),
      Edge('A', 'C')( 9),
      Edge('A', 'D')( 6), // X
      Edge('B', 'D')( 5), // X
      Edge('B', 'E')( 7),
      Edge('C', 'D')(12),
      Edge('C', 'F')( 6), // X
      Edge('D', 'E')( 4), // X
      Edge('D', 'F')( 3), // X
      Edge('D', 'G')( 7),
      Edge('E', 'G')( 2), // X
      Edge('F', 'G')( 8),
      Edge('F', 'H')(10),
      Edge('G', 'H')( 6)  // X
    )

    val mst1 = MSTKruskal[Char, Edge[Char]](allEdges)
    println(mst1.mkString("\n"))

    val numVertices     = vertices.size
    val allEdgesSorted  = allEdges.sortBy(_.weight).map { e =>
      val v1i   = vertices.indexOf(e.start)
      val v2i   = vertices.indexOf(e.end  )
      val start = if (v1i < v2i) v1i else v2i
      val end   = if (v1i < v2i) v2i else v1i
      (start << 16) | end
    }   .toArray

    implicit val rnd: Random = new Random(0L)
    val finder = new PathFinder(numVertices = numVertices, allEdgesSorted = allEdgesSorted, maxPathLen = numVertices)
    val res0  = finder.findPath(0.toShort, (numVertices - 1).toShort)
    val res   = res0.map(vertices(_))
    println(res.mkString)
  }

  def run1(): Unit = {
    val textIdx1  = 1
    val textIdx2  = 3

    val edges     = ShowSimilarities.loadAndSortGraph(textIdx1 :: textIdx2 ::Nil, mst = false)
    val edgesMST  = MSTKruskal[Vertex, SimEdge](edges)
    val edgeMap   = mkEdgeMap(edgesMST)

    implicit val rnd: Random = new Random(0L)
    val pathLen = 50
    val helper = PathHelper(1, 3, maxPathLen = pathLen)
    println("Helper ready.")

    val randomised = Random.shuffle(helper.vertices)

    randomised.sliding(2).take(20).foreach {
      case Seq(v1, v2) =>
        val seq1 = ExplorePaths.calcPath(v1, v2, edgeMap)
        println(seq1.map(_.quote).mkString("OLD: ", " -- ", ""))
        val seq2 = helper.perform(v1, v2)
        println(seq2.map(_.quote).mkString("NEW: ", " -- ", ""))
        val seq3 = helper.performExtended(v1, v2, pathLen = pathLen)
        println(seq3.map(_.quote).mkString("EXT: ", " -- ", ""))
        seq3.map(_.quote).foreach(println)
        println()
    }
  }
}