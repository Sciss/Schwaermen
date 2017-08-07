package de.sciss.schwaermen
package video

import de.sciss.numbers.{DoubleFunctions => rd}

/** @param angleStart   in radians
  * @param angleSpan    in radians
  */
final class PolarTransform(inWidth: Double, inHeight: Double, innerRadius: Double,
                    var angleStart: Double, angleSpan: Double,
                    cx: Double, cy: Double, flipX: Boolean, flipY: Boolean) {

  private[this] val Pi   = math.Pi
  private[this] val PiH  = Pi / 2
  private[this] val Pi2  = Pi * 2

  private[this] val cosTable = Array.tabulate(4096)(i => math.cos(i * Pi / 8192))

  private def cos(theta: Double): Double = {
    val w = rd.wrap(theta, 0, Pi2)
    val i = (w * 16384 / Pi2).toInt
    if (i < 8192) {
      if (i < 4096)    cosTable(i)
      else            -cosTable(8191 - i)
    } else {
      if (i < 12288)  -cosTable(i - 8192)
      else             cosTable(16383 - i)
    }
  }

  private def sin(theta: Double): Double = cos(theta - PiH)

  def apply(xs: Array[Double], off: Int): Unit =
    apply(xs(off), xs(off + 1), xs, off)

  def apply(x: Double, y: Double, out: Array[Double], outOff: Int): Unit = {
    val t     = x.toDouble / inWidth
    val theta0 = t * angleSpan + angleStart

    val theta = if (flipX) -theta0 else theta0
    val _cos  = cos(theta)
    val _sin  = sin(theta)
    val rx    = cx // if (cos >= 0) 1.0 - cx else cx
    val ry    = cy // if (sin >= 0) 1.0 - cy else cy
    val r0    = (if (flipY) inHeight - y - 1 else y) / inHeight
    val r     = innerRadius + (1.0 - innerRadius) * r0

    val px    = (cx + _cos * rx * r) * inWidth
    val py    = (cy - _sin * ry * r) * inHeight

    out(outOff)     = px
    out(outOff + 1) = py
  }
}