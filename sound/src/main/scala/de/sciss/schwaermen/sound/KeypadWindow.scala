package de.sciss.schwaermen
package sound

import java.awt.Frame
import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}

import scala.swing.Dimension

final class KeypadWindow(c: OSCClient) {
  private[this] val win = new Frame("Key Control")
  win.setResizable(false)
  win.setPreferredSize(new Dimension(400, 400))

  win.pack()
  win.setLocationRelativeTo(null)
  win.setVisible(true)

  private[this] var code  = "----"
  private[this] var muted = false

  val CodeShutdown  = "8010"
  val CodeReboot    = "8020"
  val CodeMute      = "8080"

  def codeCommitted(): Unit = code = "----"

  def enterCode(i: Int): Unit = {
    val ch = (i + 48).toChar
    code = code.substring(1) + ch
    win.setTitle(code)
    code match {
      case CodeShutdown =>
        c ! Network.OscShutdown
        codeCommitted()
      case CodeReboot =>
        c ! Network.OscReboot
        codeCommitted()
      case CodeMute     =>
        muted = !muted
        val amp = if (muted) 0f else 1f
        c ! Network.OscSetVolume(amp)
        codeCommitted()
      case _ =>
    }
  }

  win.addKeyListener(new KeyAdapter {
    override def keyPressed(e: KeyEvent): Unit = {
      val kc = e.getKeyCode
      import KeyEvent._
      if (kc >= VK_0 && kc <= VK_9)
        enterCode(kc - VK_0)
      else if (kc >= VK_NUMPAD0 && kc <= VK_NUMPAD9)
        enterCode(kc - VK_NUMPAD0)
    }
  })
  win.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = win.requestFocus()
  })

  win.requestFocus()
}