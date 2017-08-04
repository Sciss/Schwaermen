/*
 *  MyPolar.scala
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

import de.sciss.numbers

/** @param angleStart   in radians
  * @param angleSpan    in radians
  */
final class MyPolar(inWidth: Double, inHeight: Double, innerRadius: Double,
                    var angleStart: Double, angleSpan: Double,
                    cx: Double, cy: Double, flipX: Boolean, flipY: Boolean) {


  private[this] val Pi2 = math.Pi * 2

  def apply(xs: Array[Double], off: Int): Unit =
    apply(xs(off), xs(off + 1), xs, off)

  def apply(x: Double, y: Double, out: Array[Double], outOff: Int): Unit = {
    import numbers.Implicits._

    val t     = x.toDouble / inWidth
    val theta0 = (t.linlin(0, 1, angleStart, angleStart + angleSpan) + Pi2) % Pi2
    val theta = if (flipX) -theta0 else theta0
    val cos   = math.cos(theta)
    val sin   = math.sin(theta)
    val rx    = cx // if (cos >= 0) 1.0 - cx else cx
    val ry    = cy // if (sin >= 0) 1.0 - cy else cy
    val r0    = (if (flipY) inHeight - y - 1 else y) / inHeight
    val r     = innerRadius + (1.0 - innerRadius) * r0

    val px    = (cx + cos * rx * r) * inWidth
    val py    = (cy - sin * ry * r) * inHeight

    out(outOff)     = px
    out(outOff + 1) = py
  }
}