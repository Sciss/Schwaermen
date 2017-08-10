/*
 *  TextScene.scala
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
package video

import de.sciss.numbers.Implicits._
import de.sciss.schwaermen.video.TextScene.{Idle, State}

import scala.concurrent.stm.{InTxn, Ref}
import scala.swing.Swing
import scala.util.Random

object TextScene {
  private sealed trait State
  private case object  Idle extends State
}
final class TextScene(c: OSCClient, r: Random) extends Scene.Text {
  private[this] val config = c.config

  private[this] val stateRef = Ref[State](Idle)

  private[this] val videoIdx: Int = {
    val res = Network.videoDotSeq.indexOf(c.dot)
    if (res >= 0) res else {
      Console.err.println(s"WARNING: No dedicated video text for ${c.dot}. Using first instead")
      0
    }
  }

  private[this] val text: String = {
    Util.readTextResource(s"text${videoIdx + 1}.txt").replaceAll("\n", " ——— ") // XXX TODO decide here
  }

  private[this] val gl = Glyphosat(config, text, r)
  private[this] var view: TextView = _

  /* Pixels per second */
  private[this] val textSpeed     : Float = config.textVX * config.fps
  private[this] val timeOutPixels : Float = textSpeed * Network.TimeOutSeconds

  private[this] val idleTask = Ref(Option.empty[Task])

  Swing.onEDT {
    view = new TextView(config, gl)
//    view.start()
  }

  private def startInitiative()(implicit tx: InTxn): Unit = {
    idleTask() = None
  }

  def init()(implicit tx: InTxn): Unit = {
    Swing.onEDT(view.start())
    val idleDur   = r.nextFloat().linlin(0f, 1f, config.textMinDur, config.textMaxDur)
    val idleDurM  = (idleDur * 1000).toLong
    val task      = c.schedule(idleDurM)(tx => startInitiative()(tx))
    idleTask.swap(Some(task)).foreach(_.cancel())
  }
}