package de.sciss.schwaermen
package video

import scala.swing.Swing
import scala.util.Random

final class TextState(c: OSCClient, r: Random) extends State.Text {
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

  private[this] val gl = Glyphosat(c.config, text, r)
  private[this] var view: TextView = _

  Swing.onEDT {
    view = new TextView(c.config, gl)
//    view.start()
  }

  def init(): Unit = {
    Swing.onEDT(view.start())
  }
}
