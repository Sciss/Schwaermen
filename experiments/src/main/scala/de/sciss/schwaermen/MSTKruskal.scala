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