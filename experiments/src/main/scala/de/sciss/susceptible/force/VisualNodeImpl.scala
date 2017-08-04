/*
 *  VisualNodeImpl.scala
 *  (Susceptible)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.susceptible.force

import java.awt.geom.{AffineTransform, Arc2D, Ellipse2D, GeneralPath, Line2D, Rectangle2D}
import java.awt.{BasicStroke, Color, Font, Shape}

import prefuse.data.{Node => PNode}
import prefuse.render.Renderer
import prefuse.util.ColorLib
import prefuse.visual.VisualItem

import scala.swing.Graphics2D

object VisualNodeImpl {
  final val diam    = 50
  final val diam05  = 25 // diam * 0.5

  private final val eps = 1.0e-2

  final val colrPlaying   = new Color(0x00, 0xC0, 0x00)
  final val colrStopped   = new Color(0x80, 0x80, 0x80)
  final val colrBypassed  = new Color(0xFF, 0xC0, 0x00)
  final val colrSoloed    = new Color(0xFF, 0xFF, 0x00)
  final val colrMapped    = new Color(210, 60, 60)
  final val colrManual    = new Color(60, 60, 240)
  final val colrGliding   = new Color(135, 60, 150)
  final val colrAdjust    = new Color(0xFF, 0xC0, 0x00)

  final val strkThick     = new BasicStroke(2f)
  final val strkVeryThick = new BasicStroke(4f)
  final val strkDotted    = new BasicStroke(1f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10f, Array(1f, 1f), 0f)

  final val gArc          = new Arc2D.Double
  final val gLine         = new Line2D.Double

  final val margin : Double = diam * 0.2
  final val margin2: Double = margin * 2

  // final val threeDigits   = new MathContext(3, RoundingMode.HALF_UP)
}
trait VisualNodeImpl extends VisualNode /* with VisualDataImpl */ {
  import VisualNodeImpl._

  //  private[this] var _pNode: PNode = _
  //
  //  def dispose(): Unit = {
  //    if (pNode.isValid) main.graph.removeNode(pNode)
  //  }

  //  final def pNode: PNode = {
  //    if (_pNode == null) throw new IllegalStateException(s"Component $this has no initialized GUI")
  //    _pNode
  //  }

  // private[this] val _init = Ref(initialValue = false)

  //  def init(): Unit = {
  //    // implicit val itx = tx.peer
  //    // require(!_init.swap(true), s"Already initialized: $this")
  //
  //    // touch()
  //    // main.deferVisTx {
  //      mkPNode()
  //      if (Visual.DEBUG) println(s"MAKE NODE $this")
  //    // }
  //  }

  protected def mkPNode(): PNode = {
    // if (_pNode != null) throw new IllegalStateException(s"Component $this has already been initialized")
    val _pNode  = main.graph.addNode()
    val vis = main.visualization
    val vi  = vis.getVisualItem(Visual.GROUP_GRAPH, _pNode)
    vi.set(Visual.COL_MUTA, this)
    //      val sz  = nodeSize
    //      if (sz != 1.0f) vi.set(VisualItem.SIZE, sz)
    //      parent.aggr.addItem(vi)
    _pNode
  }

  protected val r: Rectangle2D = new Rectangle2D.Double()
  protected var outline: Shape = r
  protected val outerE  = new Ellipse2D.Double()
  protected val innerE  = new Ellipse2D.Double()
  protected val gp      = new GeneralPath()

  var fixed = false

  def update(shp: Shape): Unit = {
    val newR = shp.getBounds2D
    if ((math.abs(newR.getWidth  - r.getWidth ) < eps) &&
      (math.abs(newR.getHeight - r.getHeight) < eps)) {

      r.setFrame(newR.getX, newR.getY, r.getWidth, r.getHeight)
      return
    }
    r.setFrame(newR)
    outline = shp

    outerE.setFrame(0, 0, r.getWidth, r.getHeight)
    innerE.setFrame(margin, margin, r.getWidth - margin2, r.getHeight - margin2)
    gp.reset()
    gp.append(outerE, false)
    boundsResized()
  }

  private var _fontSize = 0f
  private var _font: Font = _

  protected def font: Font = _font

  def render(g: Graphics2D, vi: VisualItem): Unit = {
    //    // fixed nodes are indicated by a think white outline
    //    if (fixed) {
    //      val strkOrig = g.getStroke
    //      g.setStroke(strkVeryThick)
    //      g.setColor(ColorLib.getColor(vi.getStrokeColor))
    //      g.draw(outline)
    //      g.setStroke(strkOrig)
    //    }
    //    g.setColor(ColorLib.getColor(vi.getFillColor))
    //    g.fill(outline)
    val atOrig = g.getTransform
    g.translate(r.getX, r.getY)
    //         g.setRenderingHint( RenderingHints.KEY_FRACTIONALMETRICS, RenderingHints.VALUE_FRACTIONALMETRICS_ON )
    renderDetail(g, vi)
    g.setTransform(atOrig)
  }

  private[this] var lastFontT  : AffineTransform = _
  private[this] var lastLabel  : String = _
  private[this] var lastTextOutline: Int = _
  private[this] var labelShape : Shape = _
  private[this] var labelShapeO: Shape = _

  //    protected def drawName(g: Graphics2D, vi: VisualItem, fontSize: Float): Unit =
  //      drawLabel(g, vi, fontSize, name)

  protected def checkFont(): Unit = {
    if (_fontSize != fontSize) {
      _fontSize = fontSize
      _font = Visual.condensedFont.deriveFont(fontSize)
    }
  }

  protected def fontSize: Float

  protected def drawLabel(g: Graphics2D, vi: VisualItem, text: String): Unit = {
    checkFont()

    if (main.display.isHighQuality) {
      val frc   = g.getFontRenderContext
      val frcT  = frc.getTransform
      if (frcT != lastFontT || text != lastLabel || main.textOutline != lastTextOutline) {  // only calculate glyph vector if zoom level changes
        val txtOL = main.textOutline
        val v = _font.createGlyphVector(frc, text)
        // NOTE: there is a bug, at least with the BellySansCondensed font,
        // regarding `getVisualBounds`; it returns almost infinite width
        // for certain strings such as `"freq"`. Instead, using `getPixelBounds`
        // seems to resolve the issue.
        //
        // val vvb = v.getVisualBounds

        // NOTE: the getPixelBounds somehow incorporates wrong zoom factors.
        // The problem with `getVisualBounds` seems to originate from the
        // initial font-render-context.
        //        val vvb = if (frc.isTransformed) v.getVisualBounds else v.getPixelBounds(frc, 0f, 0f)

        // if (name == "freq") println(s"w = ${vvb.getWidth}, h = ${vvb.getHeight}; t? ${frc.isTransformed}")

        // for PDF output, drawGlyphVector gives correct font rendering,
        // while drawString only does with particular fonts.
        //         g.drawGlyphVector( v, ((r.getWidth() - vb.getWidth()) * 0.5).toFloat,
        //                           ((r.getHeight() + (fm.getAscent() - fm.getLeading())) * 0.5).toFloat )
        //         g.drawGlyphVector( v, ((r.getWidth() - vb.getWidth()) * 0.5).toFloat,
        //                               ((r.getHeight() - vb.getHeight()) * 0.5).toFloat )

        // val x = ((r.getWidth - vvb.getWidth) * 0.5).toFloat
        // val y = ((r.getHeight + vvb.getHeight) * 0.5).toFloat
        val x = 2
        val y = 12

        labelShape = v.getOutline(x, y)
        labelShapeO = if (txtOL <= 0) labelShape else {
          val strk = new BasicStroke(txtOL.toFloat)
          strk.createStrokedShape(labelShape)
        }
        lastFontT = frcT
        lastLabel = text
        lastTextOutline = txtOL
      }
      if (lastTextOutline > 0) {
        g.setColor(main.display.getBackground)
        g.fill(labelShapeO)
      }
      g.setColor(ColorLib.getColor(vi.getTextColor))
      g.fill(labelShape)

    } else {
      g.setColor(ColorLib.getColor(vi.getTextColor))
      val cx = r.getWidth  / 2
      val cy = r.getHeight / 2
      val fm = g.getFontMetrics
      g.drawString(text, (cx - (fm.stringWidth(text) * 0.5)).toInt, (cy + ((fm.getAscent - fm.getLeading) * 0.5)).toInt)
    }
  }

  protected def boundsResized(): Unit

  protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit
}

trait VisualVertexImpl extends VisualNodeImpl {
  _: VisualVertex =>

  protected def fontSize: Float = 16f

  def advance: Int = {
    checkFont()
    val fm = Renderer.DEFAULT_GRAPHICS.getFontMetrics(font)
    // fm.charWidth(character)
    fm.stringWidth(word)
  }

  def getShape(x: Double, y: Double): Shape = {
    checkFont()
    // val fm    = BoxRenderer.defaultFontMetrics
    val fm    = Renderer.DEFAULT_GRAPHICS.getFontMetrics(font)
    val w1    = fm.stringWidth(name) // dataToString(data))
    val w2    = w1 // math.max(BoxRenderer.MinBoxWidth, w1 + 6)
    // val ports = data.ports
    // val w3    = math.max(ports.numIns, ports.numOuts) * VisualPorts.MinSpacing
    val w     = w2 // math.max(w2, w3)
    r.setRect(x, y, w, BoxRenderer.DefaultBoxHeight)
    r
  }
}