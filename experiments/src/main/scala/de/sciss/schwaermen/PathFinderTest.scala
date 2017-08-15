package de.sciss.schwaermen

import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex, Word}
import de.sciss.schwaermen.ExplorePaths.mkEdgeMap

import scala.util.Random

object PathFinderTest {
  def main(args: Array[String]): Unit = {
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
    val finder = new PathFinder(numVertices = numVertices, allEdgesSorted = allEdgesSorted)
    val res0  = finder.perform(0.toShort, (numVertices - 1).toShort)
    val res   = res0.map(vertices(_))
    println(res.mkString)
  }

  def run1(): Unit = {
    val textIdx1  = 1
    val textIdx2  = 3

    val edges     = ShowSimilarities.loadGraph(textIdx1 :: textIdx2 ::Nil, mst = false)
    val edgesMST  = MSTKruskal[Vertex, SimEdge](edges)
    val edgeMap   = mkEdgeMap(edgesMST)

    val helper = PathHelper(1, 3)
    println("Helper ready.")

    val randomised = Random.shuffle(helper.vertices)

    randomised.sliding(2).take(20).foreach {
      case Seq(v1, v2) =>
//        val t1 = System.currentTimeMillis()
        val seq1 = ExplorePaths.calcPath(v1, v2, edgeMap)
//        val t2 = System.currentTimeMillis()
//        println(s"OLD took ${t2-t1}ms.")
        println(seq1.map(_.quote).mkString("OLD: ", " -- ", ""))
        val seq2 = helper.perform(v1, v2)
        println(seq2.map(_.quote).mkString("NEW: ", " -- ", ""))
    }
  }
}