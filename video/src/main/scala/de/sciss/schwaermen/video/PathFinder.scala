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
package video

import java.io.{DataInputStream, IOException}
import java.{util => ju}

import scala.annotation.tailrec
import scala.util.Random
import scala.util.control.NonFatal

object PathFinder {
  private final val COOKIE = 0x45646765 // "Edge"

  def read(textId1: Int, textId2: Int, maxPathLen: Int)(implicit rnd: Random): Meta = {
    val fis = getClass.getResourceAsStream(s"/edges$textId1${textId2}opt.bin")
    // val fis = new FileInputStream(fIn)
    try {
      val dis         = new DataInputStream(fis)
      val cookie      = dis.readInt()
      if (cookie != COOKIE)
        throw new IOException(s"Resource does not have magic cookie (expected ${COOKIE.toHexString} - found ${cookie.toHexString})")

      val textId1F    = dis.readByte  ()
      val textLen1    = dis.readShort ()
      val textId2F    = dis.readByte  ()
      val textLen2    = dis.readShort ()
      val numVertices = textLen1 + textLen2
      val numEdges    = dis.readInt   ()

      if (textId1F != textId1 || textId2F != textId2)
        throw new IOException(s"Resource does not have expected text-ids (expected $textId1,$textId2 - found $textId1F, $textId2F)")

      val edgesSorted = new Array[Int](numEdges)
      var edgeIdx     = 0
      while (edgeIdx < numEdges) {
        edgesSorted(edgeIdx) = dis.readInt()
        edgeIdx += 1
      }
      val finder = new PathFinder(numVertices = numVertices, allEdgesSorted = edgesSorted, maxPathLen = maxPathLen)
      new Meta(textId1 = textId1, textLen1 = textLen1, textId2 = textId2, textLen2 = textLen2, finder = finder)

    } finally {
      fis.close()
    }
  }

  def tryRead(textId1: Int, textId2: Int)(implicit rnd: Random): Meta =
    try {
      read(textId1 = textId1, textId2 = textId2, maxPathLen = Network.MaxPathLen)
    } catch {
      case NonFatal(ex) =>
        Console.err.println(s"Error reading path finder resource ($textId1, $textId2)")
        ex.printStackTrace()
        null
    }

  final class Meta(val textId1: Int, val textLen1: Int, val textId2: Int, val textLen2: Int,
                   val finder: PathFinder)
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
final class PathFinder(numVertices: Int, allEdgesSorted: Array[Int], val maxPathLen: Int)
                      (implicit rnd: Random) {
  // a complete graph "has n(n − 1)/2 edges (a triangular number)"
  private[this] val numEdgesComplete  = numVertices * (numVertices - 1) / 2
  private[this] val numEdges          = allEdgesSorted.length

  //  if (numEdges != numEdgesComplete) {
  //    println(s"Warning: allEdgesSorted should have length $numEdgesComplete, but instead has $numEdges.")
  //    println( "This renders `findExtendedPath` unusable.")
  //  }

  private[this] val ufParents   = new Array[Short](numVertices)
  private[this] val ufTreeSizes = new Array[Short](numVertices)

  // "If there are n vertices in the graph, then each spanning tree has n − 1 edges."
  private[this] val mst         = new Array[Int  ](numVertices - 1)

  private[this] val edgeEnabled = new Array[Boolean](numEdgesComplete)

  private def ufInit(): Unit = {
    ju.Arrays.fill(ufParents  , (-1).toShort)
    ju.Arrays.fill(ufTreeSizes,   1 .toShort)
  }

  private def ufIsConnected(from: Short, to: Short): Boolean =
    from == to || ufRoot(from) == ufRoot(to)

  // Calculates the index for the `edgeEnabled` array.
  // cf. https://stackoverflow.com/questions/27086195/linear-index-upper-triangular-matrix
  // idx = (numVertices*(numVertices-1)/2) - (numVertices-row)*((numVertices-row)-1)/2 + col - row - 1
  //     = numEdges(numVertices) - numEdges(numVertices-row) - row + (col - 1)
  // And we have row = start, col = end
  @inline
  private def indexInEdgeEnabled(start: Short, end: Short): Int = {
    val numM = numVertices - start
    numEdgesComplete - (numM * (numM - 1)) / 2 - start + (end - 1)
  }

  @inline
  private def setEdgeEnabled(start: Short, end: Short, state: Boolean): Unit = {
    val idx = indexInEdgeEnabled(start = start, end = end)
    edgeEnabled(idx) = state
  }

  @inline
  private def setEdgeEnabled(edge: Int, state: Boolean): Unit = {
    val start = (edge >> 16   ).toShort
    val end   = (edge & 0xFFFF).toShort
    val idx   = indexInEdgeEnabled(start = start, end = end)
    edgeEnabled(idx) = state
  }

  private def setVertexEnabled(vertex: Short, state: Boolean): Unit = {
    var end = vertex + 1
    while (end < numVertices) {
      setEdgeEnabled(start = vertex, end = end.toShort, state = state)
      end += 1
    }
  }

  private def setAllEdgesEnabled(): Unit =
    ju.Arrays.fill(edgeEnabled, true)

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

  // Recreates the MST.
  // Fills in `mst` and returns the number of edges.
  private def kruskal(): Int = {
    ufInit()

    var edgeIdx = 0
    var mstSize = 0
    while (edgeIdx < numEdges) {
      val edge  = allEdgesSorted(edgeIdx)
      val start = (edge >> 16   ).toShort
      val end   = (edge & 0xFFFF).toShort
      val eeIdx = indexInEdgeEnabled(start, end)
      if (edgeEnabled(eeIdx) && !ufIsConnected(start, end)) {
        ufUnion(start, end)
        mst(mstSize) = edge
        mstSize += 1
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

  private[this] val dfsPath           = new Array[Short](numVertices)
  private[this] val dfsPathI          = new Array[Short](numVertices) // same as dfsPath, but indices are dfs-local

  private[this] val extendedPath      = new Array[Short](maxPathLen)

  @inline
  private[this] def reverseEdge(edge: Int): Int = {
    val start = edge >> 16
    val end   = edge & 0xFFFF
    val rev   = end << 16 | start
    rev
  }

  @inline
  private[this] def mkEdge(v1: Short, v2: Short): Int =
    if (v1 < v2)
      (v1 << 16) | v2
    else
      (v2 << 16) | v1

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
        // we have not automatically removed the incoming edge,
        // so add an additional check here which acts as a lazy removal of that edge
        if (pathIdx == 0 || dfsPath(pathIdx - 1) != target) {
          currVertex        = target
          currVertexI       = dfsVertexIndices(target)
          pathIdx          += 1
          dfsPath (pathIdx) = currVertex
          dfsPathI(pathIdx) = currVertexI
          if (target == v2) return pathIdx + 1
        }
      }
    }

    -1  // never here
  }

  private def iterate(v1: Short, v2: Short): Int = {
    val mstLen = kruskal()
    val dfsLen = depthFirstSearch(v1 = v1, v2 = v2, mstLen = mstLen)
    dfsLen
  }

  def findPath(sourceVertex: Short, targetVertex: Short): Array[Short] = {
    require (sourceVertex != targetVertex)
    setAllEdgesEnabled()
    val dfsLen = iterate(v1 = sourceVertex, v2 = targetVertex)
    dfsPath.take(dfsLen)
  }

  private def shrinkPath(path: Array[Short], currentLen: Int, targetLen: Int): Unit = {
    var lenNow = currentLen
    while (lenNow > targetLen) {
      val idxCut1 = rnd.nextInt(lenNow - 2) + 1
      val idxCut2 = idxCut1 + 1
      System.arraycopy(path, idxCut2, path, idxCut1, lenNow - idxCut2)
      lenNow -= 1
    }
  }

  def findExtendedPath(sourceVertex: Short, targetVertex: Short, pathLen: Int): Array[Short] = {
    require (sourceVertex != targetVertex && pathLen <= maxPathLen)
    setAllEdgesEnabled()
    var dfsLen = iterate(v1 = sourceVertex, v2 = targetVertex)
    System.arraycopy(dfsPath, 0, extendedPath, 0, dfsLen)
    var extLen = dfsLen
    // now repeatedly do the following:
    // - determine a random cutting point in the current sequence `extendedPath`
    // - this gives two vertices for the cut, `v1Cut` and `v2Cut`
    // - disable the edge `(v1Cut, v2Cut)`.
    // - disable all vertices so far in the `extendedPath` except `v1Cut` and `v2Cut`.
    // - repeat Kruskal and dfs
    // - insert the new inner part of the dfs result at the cutting point
    // - until the extended path length `expLen` is at least as large as `pathLen`
    if (extLen < pathLen) {
      while (extLen < pathLen) {
        val idxCut1 = rnd.nextInt(extLen - 1)
        val idxCut2 = idxCut1 + 1
        // println(s"Path still too short ($extLen < $pathLen). Extending at cut point ($idxCut1, $idxCut2)")
        val v1Cut   = extendedPath(idxCut1)
        val v2Cut   = extendedPath(idxCut2)
        val edgeRem = mkEdge(v1Cut, v2Cut)
        setEdgeEnabled(edgeRem, state = false)

        var i = 0
        while (i < dfsLen) {
          val v3 = dfsPath(i)
          if (v3 != v1Cut && v3 != v2Cut) {
            setVertexEnabled(v3, state = false)
          }
          i += 1
        }

        dfsLen = iterate(v1 = v1Cut, v2 = v2Cut)
        val insertLen = dfsLen - 2
        // println(s"...insertLen $insertLen")
        if (insertLen > 0) {
          val testLen     = extLen + insertLen
          val insertLenL  = if (testLen <= pathLen) insertLen else {
            val lim = pathLen - extLen
            shrinkPath(dfsPath, currentLen = dfsLen, targetLen = lim + 2)
            lim
          }
          System.arraycopy(extendedPath, idxCut2, extendedPath, idxCut2 + insertLenL, extLen - idxCut2)
          System.arraycopy(dfsPath     , 1      , extendedPath, idxCut1 + 1         , insertLenL      )
          extLen += insertLenL
        }
      }
    }
    // if the extended path is now longer than
    // the requested path length, remove random inner vertices
    if (extLen > pathLen) {
      // println(s"Path too long ($extLen > $pathLen). Shrinking.")
      shrinkPath(extendedPath, currentLen = extLen, targetLen = pathLen)
      // extLen = pathLen
    }

    extendedPath.take(pathLen)
  }
}