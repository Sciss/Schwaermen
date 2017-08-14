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
import java.{util => ju}

/** Optimised algorithm for iterative DFS search in MST, given a target path length.
  * We assume that vertices are encoded as shorts, and an edge in `allEdgesSorted`
  * denotes `((v1 << 16) | v2)`, where the two vertices are strictly following the
  * consistent ordering such that `v1 < v2`.
  *
  * This algorithm is not thread-safe or reentrant.
  *
  * @param  numVertices     the number of vertices in the graph.
  * @param  allEdgesSorted  all edges in the graph sorted by weight in ascending order
  *                         an edge is given as a vertex-index pair as described above.
  *                         Since the graph is assumed to be complete, the number of
  *                         edges must be `numVertices * (numVertices - 1) / 2`.
  */
final class PathFinder(numVertices: Int, allEdgesSorted: Array[Int], maxPathLen: Int) {
  // a complete graph "has n(n − 1)/2 edges (a triangular number)"
  private[this] val numEdges    = numVertices * (numVertices - 1) / 2
  require(allEdgesSorted.length == numEdges)

  private[this] val ufParents   = new Array[Short](numVertices)
  private[this] val ufTreeSizes = new Array[Short](numVertices)

  // "If there are n vertices in the graph, then each spanning tree has n − 1 edges."
  private[this] val mst         = new Array[Int  ](numVertices - 1)

  private[this] val edgeEnabled = new Array[Boolean](numEdges)
  ju.Arrays.fill(edgeEnabled, true)

  private def ufInit(): Unit = {
    ju.Arrays.fill(ufParents  , (-1).toShort)
    ju.Arrays.fill(ufTreeSizes,   1 .toShort)
  }

  private def ufIsConnected(from: Int, to: Int): Boolean =
    from == to || ufRoot(from) == ufRoot(to)

  @tailrec
  private def ufRoot(vertex: Int): Int = {
    val p = ufParents(vertex)
    if (p < 0) vertex
    else ufRoot(p)
  }

  private def ufUnion(from: Int, to: Int): Unit = {
    val nodeA = ufRoot(from)
    val nodeB = ufRoot(to)
    // If `from` and `to` are already in the same set do nothing
    if (nodeA == nodeB) return

    val treeSizeA       = ufTreeSizes(nodeA)
    val treeSizeB       = ufTreeSizes(nodeB)
    val newTreeSize     = (treeSizeA + treeSizeB).toShort
    ufTreeSizes(nodeA)  = newTreeSize
    ufTreeSizes(nodeB)  = newTreeSize

    // Append the smaller set (sS) to the larger set (lS) by making sS representative
    // the representative of the lS. Update the tree size of the sets.
    if (treeSizeA < treeSizeB) {
      ufParents(nodeA) = nodeB.toShort
    } else {
      ufParents(nodeB) = nodeA.toShort
    }
  }

  // Recreates the MST, but aborts as soon as both
  // v1 and v2 have been seen, so we can start the DFS from here.
  // Fills in `mst` and returns the number of edges.
  private def shortKruskal(v1: Int, v2: Int): Int = {
    ufInit()

    var edgeIdx = 0
    var mstSize = 0
    var seen1   = false
    var seen2   = false
    while (edgeIdx < numEdges) {
      if (edgeEnabled(edgeIdx)) {
        val edge  = allEdgesSorted(edgeIdx)
        val start = edge >> 16
        val end   = edge & 0xFFFF
        if (!ufIsConnected(start, end)) {
          ufUnion(start, end)
          mst(mstSize) = edge
          mstSize += 1

          if (start == v1 || end == v1) {
            if (seen2) return mstSize
            seen1 = true
          }
          if (start == v2 || end == v2) {
            if (seen1) return mstSize
            seen2 = true
          }
        }
      }
      edgeIdx += 1
    }
    mstSize
  }

  // like `mst`, but sorted with start-end reversed code. only used inside `dfsInit`
  private[this] val dfsEdgesByEnd     = new Array[Int  ](numVertices - 1)
  // maps global vertex indices to dfs-used indices. only currently
  // used indices have valid values
  private[this] val dfsVertexIndices  = new Array[Short](numVertices)
  // a sorted edge map, using a zipped traversal of `mst` and `dfsEdgesByEnd`
  private[this] val dfsEdgeMap        = new Array[Int  ]((numVertices - 1) * 2)
  // for dfs-local vertex indices, the offset into dfsEdgeMap
  private[this] val dfsEdgeMapOff     = new Array[Short](numVertices)
  // for dfs-local vertex indices, the number of successive edges in dfsEdgeMap
  private[this] val dfsEdgeMapNum     = new Array[Short](numVertices)

  @inline
  private[this] def reverseEdge(edge: Int): Int = {
    val start = edge >> 16
    val end   = edge & 0xFFFF
    val rev   = end << 16 | start
    rev
  }

  // N.B. resorts `mst`. It initialises all
  // of `dfsVertexIndices`, `dfsEdgeMap`, `dfsEdgeMapOff`, `dfsEdgeMapNum`
  private def dfsInit(mstLen: Int): Unit = {
    // create a 'reversed' edge sequence
    var edgeIdx = 0
    while (edgeIdx < mstLen) {
      val edge  = mst(edgeIdx)
      val rev   = reverseEdge(edge)
      dfsEdgesByEnd(edgeIdx) = rev
      edgeIdx += 1
    }

    ju.Arrays.sort(mst          , 0, mstLen)
    ju.Arrays.sort(dfsEdgesByEnd, 0, mstLen)

    var edgeIdxRev  = 0
    var lastVertex  = -1
    var vertexIndex = -1
    var edgeMapIdx  = 0
    while (edgeIdx < mstLen && edgeIdxRev < mstLen) {
      // determine the next edge in the 'dual sorted' structure
      var edge        = 0
      var currVertex  = 0

      if (edgeIdx < mstLen) {
        val edge1 = mst(edgeIdx)
        edgeIdx += 1
        val v1 = edge1 >> 16
        if (edgeIdxRev == mstLen) {
          edge        = edge1
          currVertex  = v1
        } else {
          val edge2r  = dfsEdgesByEnd(edgeIdxRev)
          edgeIdxRev += 1
          val v2      = edge2r >> 16
          if (v1 <= v2) {
            edge        = edge1
            currVertex  = v1
          } else {
            val edge2   = reverseEdge(edge2r)
            edge        = edge2
            currVertex  = v2
          }
        }
      } else {
        val edge2r  = dfsEdgesByEnd(edgeIdxRev)
        edgeIdxRev += 1
        val v2      = edge2r >> 16
        val edge2   = reverseEdge(edge2r)
        edge        = edge2
        currVertex  = v2
      }

      // update edge-map info for current vertex

      if (currVertex == lastVertex) {
        dfsEdgeMapNum(vertexIndex) = (dfsEdgeMapNum(vertexIndex) + 1).toShort

      } else {
        vertexIndex += 1
        dfsVertexIndices(currVertex ) = vertexIndex.toShort
        dfsEdgeMapOff   (vertexIndex) = edgeMapIdx .toShort
        dfsEdgeMapNum   (vertexIndex) = 1
        lastVertex                    = currVertex
      }

      dfsEdgeMap(edgeMapIdx) = edge
      edgeMapIdx += 1
    }

    /*
          edgeMapOff    : Array[Short] --- index is vertex, value is offset in ___
          edgeMapNum    : Array[Short] --- index is vertex, value is successive length in ___
          edgeMapVisited: Array[Short] --- index is vertex, value is number of edges visited for given vertex
     */

  }
  
  private def depthFirstSearch(v1: Int, v2: Int, mstLen: Int): Unit = {
    ???
  }

  def perform(sourceVertex: Int, targetVertex: Int, pathLen: Int): Any = {
    val mstLen = shortKruskal(v1 = sourceVertex, v2 = targetVertex)
    dfsInit(mstLen)
    depthFirstSearch(v1 = sourceVertex, v2 = targetVertex, mstLen = mstLen)

//    ju.Arrays.binarySearch()


    ???
  }
}
