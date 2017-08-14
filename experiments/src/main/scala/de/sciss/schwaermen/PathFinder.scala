/*
 *  PathFinder.scala
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

import scala.annotation.tailrec

/** Optimised algorithm for iterative DFS search in MST, given a target path length.
  * We assume that vertices are encoded as shorts, and an edge in `allEdgesSorted`
  * denotes `((v1 << 16) | v2)`, where the two vertices are strictly following the
  * consistent ordering such that `v1 < v2`.
  *
  * This algorithm is not thread-safe or reentrant.
  */
final class PathFinder(numVertices: Int, allEdgesSorted: Array[Int]) {
  // a complete graph "has n(n − 1)/2 edges (a triangular number)"
  private[this] val numEdges    = numVertices * (numVertices - 1) / 2
  require(allEdgesSorted.length == numEdges)

  private[this] val ufParents   = new Array[Short](numVertices)
  private[this] val ufTreeSizes = new Array[Short](numVertices)

  // "If there are n vertices in the graph, then each spanning tree has n − 1 edges."
  private[this] val mst         = new Array[Int  ](numVertices - 1)

  private def ufInit(): Unit = {
    java.util.Arrays.fill(ufParents  , (-1).toShort)
    java.util.Arrays.fill(ufTreeSizes,   1 .toShort)
  }

  private def ufIsConnected(from: Int, to: Int): Boolean =
    from == to || ufRoot(from) == ufRoot(to)

  @tailrec
  private def ufRoot(vertex: Int): Int = {
    val p = ufParents(vertex)
    if (p < 0) vertex
    else ufRoot(p)
  }

  // recreates the MST, but aborts as soon as both
  // v1 and v2 have been seen, so we can start the DFS from here.
  private def shortKruskal(v1: Int, v2: Int): Unit = {
    ufInit()

    var edgeIdx = 0
    while (edgeIdx < numEdges) {
      edgeIdx += 1
    }
    ???
  }

  def perform(sourceVertex: Int, targetVertex: Int, pathLen: Int): Any = {

/*





 */


    ???
  }
}
