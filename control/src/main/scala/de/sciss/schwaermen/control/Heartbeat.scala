package de.sciss.schwaermen.control

import java.util.{Timer, TimerTask}

import de.sciss.osc

final class Heartbeat(c: OSCClient, period: Long = 10000L) extends TimerTask {
  private[this] val timer = new Timer("heartbeat", true)
  timer.schedule(this, 0L, period)

  def run(): Unit = c ! osc.Message("/query", "version")
}
