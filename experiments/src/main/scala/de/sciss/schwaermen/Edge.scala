/*
 *  Edge.scala
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

trait EdgeLike[+A] {
  def start : A
  def end   : A
  def weight: Double
}

final case class Edge[+A](start: A, end: A)(val weight: Double) extends EdgeLike[A] {
  override def toString: String = {
    val w1 = s"$weight"
    val w2 = f"$weight%g"
    val w3 = if (w1.length < w2.length) w1 else w2
    s"$productPrefix(start = $start, end = $end, weight = $w3)"
  }

  def updateWeight(newValue: Double): Edge[A] = Edge(start = start, end = end)(weight = newValue)
}
