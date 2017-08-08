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
import scala.swing.{Button, FlowPanel, Graphics2D, MainFrame, ToggleButton}

final class TextView(config: Config) {
  def loadText(): String = {
    Util.readTextResource("text1.txt")
  }

  private[this] val text          = loadText()
  private[this] val gl            = Glyphosat(config, text)

  private[this] var clip          = true

  private[this] val screen        = GraphicsEnvironment.getLocalGraphicsEnvironment.getDefaultScreenDevice
  private[this] val screenConf    = screen.getDefaultConfiguration
  private[this] val screenB       = screenConf.getBounds
  private[this] val NominalWidth  = 1024 // 800 // 1920
  private[this] val NominalHeight = 1024 // 400 // 1080
  private[this] val screenWidth   = screenB.width
  private[this] val screenHeight  = screenB.height
  private[this] val NominalX      = (screenWidth  - NominalWidth)/2
  private[this] val NominalY      = (screenHeight - NominalHeight)/2

  private def paintFun(g: Graphics2D): Unit = {
    g.setColor(Color.black)
    g.fillRect(0, 0, screenWidth, screenHeight)
    g.setColor(Color.white)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )

    val atOrig = g.getTransform
    if (clip) {
      g.translate(NominalX, NominalY)
      gl.render(g)
    } else {
      g.drawLine(200, 0, 200, screenHeight)
      g.drawLine(200 + NominalWidth, 0, 200 + NominalWidth, screenHeight)
      g.translate(200, 0)
      gl.render(g)
    }
    g.setTransform(atOrig)
  }

  private[this] val mainWindow = new Frame(null, screenConf) {
    setUndecorated  (true)
    setResizable    (false)

    setPreferredSize((screenWidth, screenHeight))

    override def paint(g: Graphics): Unit = {
      super.paint(g)
      paintFun(g.asInstanceOf[Graphics2D])
    }
  }

  mainWindow.pack()
  mainWindow.setLocationRelativeTo(null)
  mainWindow.setVisible(true)

  Thread.sleep(50)
  mainWindow.createBufferStrategy(2)
  Thread.sleep(50)

  private[this] val strategy = mainWindow.getBufferStrategy

  private[this] val OffScreenImg  = new BufferedImage(screenWidth, screenHeight, BufferedImage.TYPE_INT_ARGB)
  private[this] val OffScreenG    = {
    val res = OffScreenImg.createGraphics()
    res.setColor(Color.black)
    res.fillRect(0, 0, screenWidth, screenHeight)
    res
  }

  private[this] val tk      = mainWindow.getToolkit
  private[this] val fpsT    = new Array[Long](11)
  private[this] var fpsIdx  = 0

  private def tick(): Unit = {
    gl.step()

    paintFun(OffScreenG)
    do {
      do {
        val g = strategy.getDrawGraphics
        g.drawImage(OffScreenImg, 0, 0, screenWidth, screenHeight, 0, 0, screenWidth, screenHeight, null)
      } while (strategy.contentsRestored())
      strategy.show()
    } while (strategy.contentsLost())
    tk.sync()

    fpsT(fpsIdx)  = System.currentTimeMillis()
    fpsIdx        = (fpsIdx + 1) % 11
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

  private[this] val controlWindow = new MainFrame

  private[this] val ggInfo = Button("Info") {
    val dt  = fpsT((fpsIdx + 10) % 11) - fpsT(fpsIdx)  // millis-per-ten-frames
    val fps = 10000.0 / dt
    controlWindow.title = f"$fps%g fps"
  }

  private def eject(): Unit = {
    val v = gl.lastWord
    if (v != null) v.eject = true
  }

  private[this] val ggEject = Button("Eject")(eject())

  private[this] val pBottom = new FlowPanel(ggAnim, ggTick, ggEject, ggClip, ggInfo)

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
        case KeyEvent.VK_ESCAPE => quit()
        case KeyEvent.VK_E      => eject()
        case KeyEvent.VK_C      => controlWindow.open()

        case _ =>
      }
    }
  })
  mainWindow.addMouseListener(new MouseAdapter {
    override def mousePressed(e: MouseEvent): Unit = mainWindow.requestFocus()
  })

  // "hide" cursor
  private[this] val cursorImg = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
  private[this] val cursor    = mainWindow.getToolkit.createCustomCursor(cursorImg, new Point(0, 0), "blank")
  mainWindow.setCursor(cursor)

  screen.setFullScreenWindow(mainWindow)
  mainWindow.requestFocus()

  def run(): Unit = {
    startAnim()
  }

  def quit(): Unit = sys.exit()

//  def runOLD(): Unit = {
//    val text    = loadText()
//
//    val tmpImg  = new BufferedImage(16, 16, BufferedImage.TYPE_INT_ARGB)
//    val tmpG    = tmpImg.createGraphics()
//    val font    = Glyphosat.mkFont(64f)
//    val fm      = tmpG.getFontMetrics(font)
//    val frc     = fm.getFontRenderContext
//    val gv      = font.createGlyphVector(frc, text.take(32))
//    val shape   = gv.getOutline
//    val rectIn  = shape.getBounds
//
//    val extentIn = math.max(rectIn.width, rectIn.height) * 0.33 // 0.2515
//
//    val p = new PolarTransform(inWidth = extentIn /* rectIn.width */, inHeight = extentIn /* rectIn.height */,
//      innerRadius = 0.0, angleStart = 0.0, angleSpan = math.Pi * 0.5,
//      cx = extentIn * 0.5 /* rectIn.width */ * 0.5 /* rectIn.getCenterX */,
//      cy = extentIn * 0.5 /* rectIn.height */ * 0.5 /* rectIn.getCenterY */, flipX = true, flipY = true)
//
//    val arr   = new Array[Double](6)
//    val path  = new Path2D.Double()
//    var angle = 0.0
//
//    def updatePath(): Unit = {
//      path.reset()
//      p.angleStart = angle
//      val it    = shape.getPathIterator(AffineTransform.getTranslateInstance(-rectIn.x, -rectIn.y))
//
//      while (!it.isDone) {
//        val tpe = it.currentSegment(arr)
//        (tpe: @switch) match {
//          case PathIterator.SEG_MOVETO  =>
//            p(arr, 0)
//            path.moveTo(arr(0), arr(1))
//
//          case PathIterator.SEG_LINETO  =>
//            p(arr, 0)
//            path.lineTo(arr(0), arr(1))
//
//          case PathIterator.SEG_QUADTO  =>
//            p(arr, 0)
//            p(arr, 2)
//            path.quadTo(arr(0), arr(1), arr(2), arr(3))
//
//          case PathIterator.SEG_CUBICTO =>
//            p(arr, 0)
//            p(arr, 2)
//            p(arr, 4)
//            path.curveTo(arr(0), arr(1), arr(2), arr(3), arr(4), arr(5))
//
//          case PathIterator.SEG_CLOSE   =>
//            path.closePath()
//        }
//        it.next()
//      }
//    }
//
//    updatePath()
//    val rectOut = path.getBounds
//    //    println(rectOut)
//
//    val comp = new Component {
//      background = Color.black
//      foreground = Color.white
//      opaque     = true
//
//      preferredSize = {
//        val d = rectOut.getSize
//        d.width  += 8
//        d.height += 8
//        d.width   = math.min(1290, d.width)
//        d.height  = math.min(1080, d.height)
//        d
//      }
//
//      override protected def paintComponent(g: Graphics2D): Unit = {
//        super.paintComponent(g)
//        val w = peer.getWidth
//        val h = peer.getHeight
//        g.setColor(background)
//        g.fillRect(0, 0, w, h)
//        g.setColor(foreground)
//        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON )
//        g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE  )
//
//        //          val sx = w.toDouble / rectOut.width
//        //          val sy = h.toDouble / rectOut.height
//        val sx = (w - 8).toDouble / rectOut.getMaxX
//        val sy = (h - 8).toDouble / rectOut.getMaxY
//        val scale = math.min(sx, sy)
//        g.translate(4, 4)
//        g.scale(scale, scale)
//        //          g.translate(4 - rectOut.x , 4 - rectOut.y)
//        g.fill(path)
//      }
//    }
//
//    new MainFrame {
//      contents = comp
//      pack().centerOnScreen()
//      open()
//    }
//
//    val Pi2     = math.Pi * 2
//    val angStep = -0.2 * math.Pi / 180 + Pi2
//    //    val tk      = comp.peer.getToolkit
//
//    val t = new javax.swing.Timer(30, Swing.ActionListener { _ =>
//      angle = (angle + angStep) % Pi2
//      updatePath()
//      comp.repaint()
//      // tk.sync()
//    })
//    t.start()
//  }
}