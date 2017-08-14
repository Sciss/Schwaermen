package de.sciss.schwaermen

import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import de.sciss.schwaermen.ExplorePaths.mkEdgeMap

import scala.util.Random

object PathFinderTest {
  def main(args: Array[String]): Unit = {
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