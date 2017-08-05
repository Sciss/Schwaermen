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

package de.sciss.schwaermen.sound

import java.util.{Timer, TimerTask}

import de.sciss.osc

final class Heartbeat(c: OSCClient, period: Long = 30000L) extends TimerTask {
  private[this] val timer = new Timer("heartbeat", true)
  timer.schedule(this, period, period)

  def run(): Unit = c ! osc.Message("/heart")
}
