/*
 *  UnionFind.scala
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
import scala.collection.breakOut

final case class Node[A](parent: Option[A], treeSize: Int)

// this is based on code by Rafael O. Torres, which
// however was wrong.
//
// Original code, licensed under MIT:
// https://github.com/rators/PriorityMap/blob/master/src/main/scala/utils/algorithm/kruskal/UnionFind.scala

object UnionFind {
  def apply[A: Ordering, E <: EdgeLike[A]](edges: Iterable[E]): UnionFind[A] = {
    val indexMap  = vertexIndexMap[A, E](edges)
    val size      = indexMap.size
    val nodes     = Vector.fill(size)(Node[A](None, 1))

    new UnionFind[A](nodes, indexMap)
  }

  def vertexIndexMap[A, E <: EdgeLike[A]](edges: Iterable[E])(implicit ord: Ordering[A]): Map[A, Int] = {
    val flat  : List[A] = edges.flatMap(edge => List(edge.start, edge.end))(breakOut)
    val sorted: List[A] = flat.distinct.sorted
    sorted.iterator.zipWithIndex.toMap
  }
}

/**
  * Purely functional union find implementation.
  */
final class UnionFind[A](nodes: Vector[Node[A]], indexMap: Map[A, Int]) {

  def union(from: A, to: A): UnionFind[A] = {
    // if `from` == `to` then `from` and `to` are in the same set then
    // do nothing an return this set. Connected returns true
    // before utilizing the data structure to check connectivity.
    if (from == to) return this

    val rootA = root(from)
    val rootB = root(to)

    // If `from` and `to` are already in the same set do nothing and return
    // this instance of union-find.
    if (rootA == rootB) return this

    val idxA  = indexMap(rootA)
    val idxB  = indexMap(rootB)
    val nodeA = nodes(idxA)
    val nodeB = nodes(idxB)

    val newTreeSize = nodeA.treeSize + nodeB.treeSize

    // Append the smaller set (sS) to the larger set (lS) by making sS representative
    // the representative of the lS. Update the tree size of the sets.
    val (newNodeA, newNodeB) = if (nodeA.treeSize < nodeB.treeSize) {
      val newNodeA = Node[A](Some(rootB) , newTreeSize)
      val newNodeB = nodeB.copy(treeSize = newTreeSize)
      (newNodeA, newNodeB)
    } else {
      val newNodeA = nodeA.copy(treeSize = newTreeSize)
      val newNodeB = Node[A](Some(rootA) , newTreeSize)
      (newNodeA, newNodeB)
    }

    val newNodes = nodes
      .updated(idxA, newNodeA)
      .updated(idxB, newNodeB)

    new UnionFind[A](newNodes, indexMap)
  }

  def isConnected(from: A, to: A): Boolean =
    from == to || root(from) == root(to)

  @tailrec
  private def root(vertex: A): A = {
    val idx = indexMap(vertex)
    nodes(idx).parent match {
      case None         => vertex
      case Some(parent) => root(parent)
    }
  }

  /** For hypothesis testing */
  def pathToRoot(vertex: A): List[A] = {
    @tailrec
    def loop(child: A, res: List[A]): List[A] = {
      val res1  = child :: res
      val idx   = indexMap(child)
      nodes(idx).parent match {
        case None         => res1
        case Some(parent) => loop(parent, res1)
      }
    }

    loop(vertex, Nil)
  }
}