/*
 *  MSTKruskal.scala
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

import scala.annotation.tailrec

object MSTKruskal {
  def apply[A: Ordering, E <: EdgeLike[A]](edges: List[E]): List[E] = {
    @tailrec
    def loop(unionFind: UnionFind[A], rem: List[E], res: List[E]): List[E] =
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

        case Nil => res
      }

    val sorted  = edges.sortBy(_.weight)
    val xs      = loop(UnionFind(edges), rem = sorted, res = Nil)
    xs.reverse
  }
}