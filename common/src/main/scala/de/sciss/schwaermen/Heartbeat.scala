/*
 *  Heartbeat.scala
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

import java.util.TimerTask

final class Heartbeat(c: OSCClientLike) extends TimerTask {
  private[this] val period: Long = (Network.HeartPeriodSeconds * 1000).toLong

  c.timer.schedule(this, 1000L, period)

  def run(): Unit = c ! Network.OscHeart
}
