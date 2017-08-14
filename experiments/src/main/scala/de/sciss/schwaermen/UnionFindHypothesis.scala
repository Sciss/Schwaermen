package de.sciss.schwaermen

import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import de.sciss.schwaermen.ExplorePaths.{WEIGHT_POW, mkEdgeMap}

import scala.annotation.tailrec
import scala.util.Random

/** TL;DR - this hypothesis didn't hold */
object UnionFindHypothesis {
  def main(args: Array[String]): Unit = {
    val edges           = ShowSimilarities.loadGraph(1::3::Nil, weightPow = WEIGHT_POW, dropAmt = 0.0, mst = false)
    val (edgesMST, uf)  = kruskalWithUF[Vertex, SimEdge](edges)
    val edgeMap         = mkEdgeMap(edgesMST)
    val vertices        = edgeMap.keysIterator.toVector
    val randomised      = Random.shuffle(vertices)

    randomised.sliding(2).take(20).foreach {
      case Seq(v1, v2) =>
        val seq1 = ExplorePaths.calcPath(v1, v2, edgeMap)
        println(seq1.map(_.quote).mkString("OLD: ", " -- ", ""))
        /* val seq2 = */ hypothesis(v1, v2, uf)
//        println(seq2.map(_.quote).mkString("NEW: ", " -- ", ""))
    }
  }

  def hypothesis(v1: Vertex, v2: Vertex, uf: UnionFind[Vertex]): List[Vertex] = {
    val p1      = uf.pathToRoot(v1)
    val p2      = uf.pathToRoot(v2)

    println(p1.map(_.quote).mkString("P1 : ", " -- ", ""))
    println(p2.map(_.quote).mkString("P2 : ", " -- ", ""))

    val p1r     = p1.reverse
    val p2r     = p2.reverse
    val common  = (p1r zip p2r).count(tup => tup._1 == tup._2)
//    assert(common > 0)
    val seq     = p1 ::: p2r.drop(common)
    seq
  }

  def kruskalWithUF[A: Ordering, E <: EdgeLike[A]](edges: List[E]): (List[E], UnionFind[A]) = {
    @tailrec
    def loop(unionFind: UnionFind[A], rem: List[E], res: List[E]): (List[E], UnionFind[A]) =
      rem match {
        case edge :: tail =>
          import edge._
          if (!unionFind.isConnected(start, end)) {
            val newUnion  = unionFind.union(start, end)
            val newRes    = edge :: res
            loop(newUnion , rem = tail, res = newRes)
          } else {
            loop(unionFind, rem = tail, res = res   )
          }

        case Nil => res -> unionFind
      }

    val sorted  = edges.sortBy(_.weight)
    val (xs, ufFinal) = loop(UnionFind(edges), rem = sorted, res = Nil)
    val xsr = xs.reverse
    xsr -> ufFinal
  }
}
