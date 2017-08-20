/*
 *  TextView.scala
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

import java.awt.event.{KeyAdapter, KeyEvent, MouseAdapter, MouseEvent}
import java.awt.image.BufferedImage
import java.awt.{Color, Frame, Graphics, GraphicsEnvironment, Point, RenderingHints}
import java.util.TimerTask

import scala.swing.Swing._
import scala.swing.event.ButtonClicked
import scala.swing.{Button, FlowPanel, Graphics2D, ToggleButton}

final class TextView(config: Config, gl: Glyphosat, videoId: Int) {
  private[this] var clip          = true

  private[this] val screen        = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  private[this] val screenConf    = screen.getDefaultConfiguration
  private[this] val screenB       = screenConf.getBounds
  private[this] val NominalWidth  = if (config.smallWindow) 512 else 1024 // 800 // 1920
  private[this] val NominalHeight = if (config.smallWindow) 512 else 1024 // 400 // 1080
  private[this] val screenWidth   = screenB.width
  private[this] val screenHeight  = screenB.height
  private[this] val NominalX      = (screenWidth  - NominalWidth )/2
  private[this] val NominalY      = (screenHeight - NominalHeight)/2

  private[this] val OffScreenW    = if (config.smallWindow) NominalWidth  else screenWidth
  private[this] val OffScreenH    = if (config.smallWindow) NominalHeight else screenHeight
  private[this] val clipExtent    = 200 // if (config.smallWindow) 100 else 200

  private[this] val isFullScreen = !config.smallWindow

  private def paintFun(g: Graphics2D): Unit = {
    g.setColor(Color.black)
    g.fillRect(0, 0, screenWidth, screenHeight)
    g.setColor(Color.white)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )

    val atOrig = g.getTransform
    if (isFullScreen) {
      if (clip) {
        g.translate(NominalX, NominalY)
        gl.render(g)
      } else {
        g.drawLine(clipExtent, 0, clipExtent, OffScreenH)
        g.drawLine(clipExtent + NominalWidth, 0, clipExtent + NominalWidth, OffScreenH)
        g.translate(clipExtent, 0)
        gl.render(g)
      }
    } else {
      g.scale(0.5, 0.5)
      gl.render(g)
    }
    g.setTransform(atOrig)
  }

  private[this] val mainWindow = new Frame(null, screenConf) {
    if (isFullScreen) {
      setUndecorated(true)
    } else {
      setTitle(s"video ${videoId + 1}")
    }

    setResizable(false)

    if (isFullScreen)
      setPreferredSize((screenWidth, screenHeight))
    else
      setPreferredSize((NominalWidth, NominalHeight))

    override def paint(g: Graphics): Unit = {
      super.paint(g)
      paintFun(g.asInstanceOf[Graphics2D])
    }
  }

  mainWindow.pack()
  mainWindow.setLocationRelativeTo(null)
  if (!isFullScreen) {
    val pad = (screenWidth - 3 * NominalWidth) / 2
    val x   = videoId * (NominalWidth + pad)
    mainWindow.setLocation(x, mainWindow.getY)
  }
  mainWindow.setVisible(true)

  private[this] val strategy = {
    Thread.sleep(50)
    mainWindow.createBufferStrategy(2)
    Thread.sleep(50)
    mainWindow.getBufferStrategy
  }

  private[this] val OffScreenImg  =
    new BufferedImage(OffScreenW, OffScreenH, BufferedImage.TYPE_INT_ARGB)

  private[this] val OffScreenG    = {
    val res = OffScreenImg.createGraphics()
    res.setColor(Color.black)
    res.fillRect(0, 0, OffScreenW, OffScreenH)
    res
  }

  private[this] val tk = mainWindow.getToolkit

  private def tick(): Unit = {
    gl.step()

    paintFun(OffScreenG)
    do {
      do {
        val g = strategy.getDrawGraphics
        g.drawImage(OffScreenImg, 0, 0, OffScreenW, OffScreenH, 0, 0, OffScreenW, OffScreenH, null)
      } while (strategy.contentsRestored())
      strategy.show()
    } while (strategy.contentsLost())
    tk.sync()
  }

  private[this] var t = Option.empty[java.util.Timer]

  private def ToggleButton(title: String)(fun: Boolean => Unit): ToggleButton =
    new ToggleButton(title) {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => fun(selected)
      }
    }

  private def startAnim(): Unit = {
    val dly = 1000 / config.fps
    val tim = new java.util.Timer("animation")
    val tt  = new TimerTask {
      def run(): Unit = tick()
    }
    tim.schedule(tt, 0L, dly)
    t = Some(tim)
  }

  private[this] val ggAnim = ToggleButton("Anim") { selected =>
    t.foreach(_.cancel())
    t = None
    if (selected) startAnim()
  }
  ggAnim.selected = true

  private[this] val ggClip = ToggleButton("Clip") { selected =>
    clip = selected
  }
  ggClip.selected = clip

  private[this] val ggTick = Button("Tick")(tick())

  private[this] val controlWindow = new scala.swing.Frame {
    override def closeOperation(): Unit = if (isFullScreen) sys.exit()
  }

  private[this] val ggInfo = Button("Info") {
    val fps = gl.fps
    controlWindow.title = f"$fps%g fps"
  }

//  private def testEject(): Unit = {
//    val v = gl.lastWord
//    if (v != null) v.eject = true
//  }

//  private[this] val ggEject = Button("Eject")(testEject())

  private[this] val pBottom = new FlowPanel(ggAnim, ggTick, /* ggEject, */ ggClip, ggInfo)

  {
    import controlWindow._

    contents = pBottom
    resizable = false
    pack().centerOnScreen()
    location = (location.x, 40)
  }

  mainWindow.addKeyListener(new KeyAdapter {
    override def keyTyped  (e: KeyEvent): Unit = ()
    override def keyPressed(e: KeyEvent): Unit = {
      e.getKeyCode match {
        case KeyEvent.VK_ESCAPE => if (isFullScreen) quit()
//        case KeyEvent.VK_E      => testEject()
        case KeyEvent.VK_C      => controlWindow.open()

        case _ =>
      }
    }
  })
  mainWindow.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = mainWindow.requestFocus()
  })

  if (isFullScreen) {
    // "hide" cursor
    val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
    val cursor    = mainWindow.getToolkit.createCustomCursor(cursorImg, new Point(0, 0), "blank")
    mainWindow.setCursor(cursor)
    screen.setFullScreenWindow(mainWindow)
  }

  mainWindow.requestFocus()

  def start(): Unit = {
    startAnim()
  }

  def quit(): Unit = sys.exit()
}