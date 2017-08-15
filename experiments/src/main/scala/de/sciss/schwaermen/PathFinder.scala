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

import java.{util => ju}

import de.sciss.kollflitz.Vec
import de.sciss.schwaermen.BuildSimilarities.Vertex

import scala.annotation.tailrec
import scala.collection.breakOut

/** Transition before we store the vertices more efficiently */
object PathHelper {
  def apply(textIdx1: Int, textIdx2: Int): PathHelper = {
    // they'll come out sorted by similarity, so we have to reverse that
    val simEdge         = ShowSimilarities.loadGraph(textIdx1 :: textIdx2 :: Nil, mst = false)
    val simEdgeR        = simEdge.reverse
    // sorting will be with all textIdx1 first, followed by all textIdx2 (Vertex.Ord)
    val allVertices: Vec[Vertex] = {
      val tmp: Vec[Vertex] = simEdgeR.flatMap(e => e.start :: e.end :: Nil)(breakOut)
      tmp.distinct.sorted
    }

    val numVertices     = allVertices.size
    println(s"numVertices = $numVertices")
    val numText1        = allVertices.indexWhere(_.textIdx == textIdx2)
    require(numText1 > 0)
    val numText2        = numVertices - numText1
    val vertexIndexMap  = allVertices.zipWithIndex.toMap

    val allEdgesSorted: Array[Int] = simEdgeR.map { e =>
      val v1i   = vertexIndexMap(e.start)
      val v2i   = vertexIndexMap(e.end  )
      val start = if (v1i < v2i) v1i else v2i
      val end   = if (v1i < v2i) v2i else v1i
      (start << 16) | end
    } (breakOut)

    val finder = new PathFinder(numVertices = numVertices, allEdgesSorted = allEdgesSorted)
    new PathHelper(finder = finder, vertices = allVertices, vertexIndexMap = vertexIndexMap,
      numText1 = numText1, numText2 = numText2)
  }
}
final class PathHelper(val finder: PathFinder, val vertices: Vec[Vertex], val vertexIndexMap: Map[Vertex, Int],
                       val numText1: Int, val numText2: Int) {
  def perform(v1: Vertex, v2: Vertex /* , pathLen: Int */): List[Vertex] = {
    val t1  = System.currentTimeMillis()
    val arr = finder.perform(sourceVertex = vertexIndexMap(v1).toShort,
      targetVertex = vertexIndexMap(v2).toShort /* , pathLen = pathLen */)
    val t2  = System.currentTimeMillis()
    println(s"Took ${t2-t1}ms.")
    arr.map(vertices(_))(breakOut)
  }
}

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
final class PathFinder(numVertices: Int, allEdgesSorted: Array[Int] /* , maxPathLen: Int */) {
  // a complete graph "has n(n − 1)/2 edges (a triangular number)"
//  private[this] val numEdgesComplete = numVertices * (numVertices - 1) / 2

  private[this] val numEdges    = allEdgesSorted.length

// XXX TODO --- not that currently we do not provide all edges,
// because we skipped those between 'sub-phrases'.
// this is fine until we want to efficiently toggle edgeEnabled

//  require(numEdges == numEdgesComplete,
//    s"allEdgesSorted should have length numEdgesComplete, but instead has $numEdges")

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

  private def ufIsConnected(from: Short, to: Short): Boolean =
    from == to || ufRoot(from) == ufRoot(to)

  @tailrec
  private def ufRoot(vertex: Short): Int = {
    val p = ufParents(vertex)
    if (p < 0) vertex
    else ufRoot(p)
  }

  private def ufUnion(from: Short, to: Short): Unit = {
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
  private def shortKruskal(v1: Short, v2: Short): Int = {
    ufInit()

    var edgeIdx = 0
    var mstSize = 0
    var seen1   = false
    var seen2   = false
    while (edgeIdx < numEdges) {
      if (edgeEnabled(edgeIdx)) {
        val edge  = allEdgesSorted(edgeIdx)
        val start = (edge >> 16   ).toShort
        val end   = (edge & 0xFFFF).toShort
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

  private[this] val dfsPath           = new Array[Short](numVertices - 1)
  private[this] val dfsPathI          = new Array[Short](numVertices - 1) // same as dfsPath, but indices are dfs-local

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

    edgeIdx         = 0
    var edgeIdxRev  = 0
    var lastVertex  = -1
    var vertexIndex = -1
    var edgeMapIdx  = 0
    while (edgeIdx < mstLen || edgeIdxRev < mstLen) {
      // determine the next edge in the 'dual sorted' structure
      var edge        = 0
      var currVertex  = 0

      if (edgeIdx < mstLen) {
        val edge1 = mst(edgeIdx)
        if (edgeIdxRev == mstLen) {
          edgeIdx += 1
          edge        = edge1
          currVertex  = edge1 >> 16
        } else {
          val edge2r  = dfsEdgesByEnd(edgeIdxRev)
          if (edge1 <= edge2r) {
            edgeIdx += 1
            edge        = edge1
            currVertex  = edge1 >> 16
          } else {
            edgeIdxRev += 1
            val edge2   = reverseEdge(edge2r)
            edge        = edge2
            currVertex  = edge2r >> 16
          }
        }
      } else {
        val edge2r  = dfsEdgesByEnd(edgeIdxRev)
        edgeIdxRev += 1
        val edge2   = reverseEdge(edge2r)
        edge        = edge2
        currVertex  = edge2r >> 16
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

//  private def dfsVertexIndex(global: Int): Int = {
//    val res = ju.Arrays.binarySearch(dfsVertexIndices, global.toShort)
//    assert(res >= 0)
//    res
//  }

  // Returns the DFS path len, the path itself is found in `dfsPath`
  private def depthFirstSearch(v1: Short, v2: Short, mstLen: Int): Int = {
    dfsInit(mstLen)
    val v1i = dfsVertexIndices(v1)

    var pathIdx       = 0
    var currVertex    = v1
    var currVertexI   = v1i
    dfsPath (0)       = v1
    dfsPathI(0)       = v1i

    while (true) {
      val numUnseen = dfsEdgeMapNum(currVertexI)
      if (numUnseen == 0) {
        // backtrack
        assert(pathIdx > 0)
        pathIdx -= 1
        currVertex   = dfsPath (pathIdx)
        currVertexI  = dfsPathI(pathIdx)

      } else {
        val numU1         = numUnseen - 1
        dfsEdgeMapNum(currVertexI) = numU1.toShort
        val edgeMapIdx    = dfsEdgeMapOff(currVertexI) + numU1
        val edge          = dfsEdgeMap(edgeMapIdx)
        val start         = (edge >> 16   ).toShort
        val end           = (edge & 0xFFFF).toShort
        val target        = if (start == currVertex) end else start
        currVertex        = target
        currVertexI       = dfsVertexIndices(target)
        pathIdx          += 1
        dfsPath (pathIdx) = currVertex
        dfsPathI(pathIdx) = currVertexI
        if (target == v2) return pathIdx + 1
      }
    }

    -1  // never here
  }

  def perform(sourceVertex: Short, targetVertex: Short /* , pathLen: Int */): Array[Short] = {
    require (sourceVertex != targetVertex)
//    val mstLen = shortKruskal(v1 = sourceVertex, v2 = targetVertex)
    val mstLen = shortKruskal(v1 = -1, v2 = -1)
    println(s"mstLen = $mstLen")
    val dfsLen = depthFirstSearch(v1 = sourceVertex, v2 = targetVertex, mstLen = mstLen)
    dfsPath.take(dfsLen)
  }
}
