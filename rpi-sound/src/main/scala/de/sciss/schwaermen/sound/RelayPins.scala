/*
 *  RelayPins.scala
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

package de.sciss.schwaermen.sound

object RelayPins {
  final val default: RelayPins = new RelayPins(
    left  = Vector( 2,  3, 14, 15, 17, 18),
    right = Vector(22, 23, 10,  9, 25, 11)
  )
}
final case class RelayPins(left: Vector[Int], right: Vector[Int]) {
  def replace(before: Int, now: Int): RelayPins = {
    val idxL = left.indexOf(before)
    if (idxL >= 0) copy(left = left.updated(idxL, now))
    else {
      val idxR = right.indexOf(before)
      if (idxR < 0) throw new IllegalArgumentException(s"Pin $before is not used in $this")
      copy(right = right.updated(idxR, now))
    }
  }
}