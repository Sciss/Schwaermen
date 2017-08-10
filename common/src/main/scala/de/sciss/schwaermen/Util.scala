/*
 *  Util.scala
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

import scala.util.Random

object Util {
  def shutdown(): Unit = {
    import sys.process._
    Seq("sudo", "shutdown", "now").run()
  }

  def reboot(): Unit = {
    import sys.process._
    Seq("sudo", "reboot", "now").run()
  }

  private[this] val uniqueSync  = new AnyRef
  private[this] var uniqueID    = 0

  def nextUniqueID(): Int = uniqueSync.synchronized {
    val res = uniqueID
    uniqueID += 1
    res
  }

  def readTextResource(name: String): String = {
    val url = getClass.getResource(name)
    require(url != null)
    val is  = url.openStream()
    val sz  = is.available()
    val arr = new Array[Byte](sz)
    is.read(arr)
    is.close()
    new String(arr, "UTF-8")
  }

  // rng ops

  def exprand(lo: Float, hi: Float)(implicit random: Random): Float =
    lo * math.exp(math.log(hi / lo) * random.nextFloat).toFloat

  def rrand(lo: Float, hi: Float)(implicit random: Random): Float =
    random.nextFloat() * (hi - lo) + lo

  /** `lo` to `hi` (inclusive). */
  def rrand(lo: Int, hi: Int)(implicit random: Random): Int = {
    if (lo <= hi) {
      random.nextInt(hi - lo + 1) + lo
    } else {
      random.nextInt(lo - hi + 1) + hi
    }
  }

  /** `0 to (i-1)` or `(0 until i)` (exclusive) */
  def rand(i: Int)(implicit random: Random): Int = random.nextInt(i)

  /** `0` until `d` (exclusive). */
  def rand(d: Float)(implicit random: Random): Float = random.nextFloat() * d

  def coin(w: Float = 0.5f)(implicit random: Random): Boolean = random.nextFloat() < w

  def choose[A](seq: Seq[A])(implicit random: Random): A =
    seq(random.nextInt(seq.size))
}