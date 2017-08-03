/*
 *  MyDisplay.scala
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

import java.awt.{Graphics, Graphics2D}

import prefuse.data.expression.Predicate
import prefuse.{Display, Visualization}

class MyDisplay(visualization: Visualization, predicate: Predicate)
  extends Display(visualization, predicate) {

  def this(visualization: Visualization) = this(visualization, null)

  def this() = this(null)

  /**
    * Paints the offscreen buffer to the provided graphics context.
    * @param g the Graphics context to paint to
    */
//  override protected def paintBufferToScreen(g: Graphics): Unit = this.synchronized {
//    // g.drawImage(m_offscreen, 0, 0, null)
//    g.drawImage(m_offscreen, 0, 0, getWidth, getHeight, null)
//  }

  private[this] var mark            = -1L
  private[this] val sampleInterval  = 10

  private var _bufWidth  = 0
  private var _bufHeight = 0

  def bufWidth : Int = if (_bufWidth  > 0) _bufWidth  else getWidth
  def bufHeight: Int = if (_bufHeight > 0) _bufHeight else getHeight

  def bufWidth_=(value: Int): Unit = if (_bufWidth != value) {
    _bufWidth   = value
    m_offscreen = null
  }

  def bufHeight_=(value: Int): Unit = if (_bufHeight != value) {
    _bufHeight  = value
    m_offscreen = null
  }

  override def paintComponent(g: Graphics): Unit = {
    if (m_offscreen == null) {
      m_offscreen = getNewOffscreenBuffer(bufWidth, bufHeight)
      damageReport()
    }
    val g2D     = g.asInstanceOf[Graphics2D]
    val buf_g2D = m_offscreen.getGraphics.asInstanceOf[Graphics2D]
    val sx      = bufWidth .toDouble / getWidth
    val sy      = bufHeight.toDouble / getHeight
    if (sx != 1.0 || sy != 1.0) buf_g2D.scale(sx, sy)
    paintDisplay(buf_g2D, getSize)
    paintBufferToScreen(g2D)
    firePostPaint(g2D)
    buf_g2D.dispose()
    nframes += 1
    if (mark < 0) {
      mark      = System.currentTimeMillis()
      nframes   = 0
    }
    else if (nframes == sampleInterval) {
      val t     = System.currentTimeMillis()
      frameRate = (1000.0 * nframes) / (t - mark)
      mark      = t
      nframes   = 0
    }
  }

  //  private[this] val m_clipScaled = new Rectangle2D.Double

  /*
   * Renders the display within the given graphics context and size bounds.
   * @param g2D the <code>Graphics2D</code> context to use for rendering
   * @param d the rendering width and height of the Display
   */
  /*
  def paintDisplay1(g2D: Graphics2D, d: Dimension, sx: Double, sy: Double): Unit = m_vis.synchronized {
    this.synchronized {
      if (m_clip.isEmpty) return
      m_screen.setClip(0, 0, d.width + 1, d.height + 1)
      m_screen.transform(m_itransform)
      val pixel = 1.0 + 1.0 / getScale

      if (m_damageRedraw) {
        if (m_clip.isInvalid)
          m_clip.setClip(m_screen)
        else
          m_clip.intersection(m_screen)

        m_clip.expand(pixel)
        prepareGraphics(g2D)
        m_rclip.setFrameFromDiagonal(m_clip.getMinX, m_clip.getMinY, m_clip.getMaxX, m_clip.getMaxY)
        val clipScaled = if (sx == 1 && sy == 1) m_rclip else {
          m_clipScaled.setRect(m_rclip.getX * sx, m_rclip.getY * sy, m_rclip.getWidth * sx, m_rclip.getHeight * sy)
          m_clipScaled
        }
        g2D.setClip(clipScaled) // correct? or is affine transform applied anyways?
        m_rclip.setFrameFromDiagonal(m_clip.getMinX - pixel, m_clip.getMinY - pixel, m_clip.getMaxX + pixel, m_clip.getMaxY + pixel)

      } else {
        m_rclip.setFrame(m_screen.getMinX, m_screen.getMinY, m_screen.getWidth, m_screen.getHeight)
        m_clip.setClip(m_screen)
        prepareGraphics(g2D)
      }

      clearRegion(g2D, m_rclip)
      getItemBounds(m_rclip)
      m_bounds.reset()
      m_queue.clear()
      val items: java.util.Iterator[_] = m_vis.items(m_predicate)

      m_visibleCount = 0
      while (items.hasNext) {
        val item    = items.next.asInstanceOf[VisualItem]
        val bounds  = item.getBounds
        m_bounds.union(bounds)
        if (m_clip.intersects(bounds, pixel)) m_queue.addToRenderQueue (item)
        if (item.isInteractive)               m_queue.addToPickingQueue(item)

        m_visibleCount += 1
      }

      m_queue.sortRenderQueue()

      var i = 0
      while (i < m_queue.rsize) {
        m_queue.ritems(i).render(g2D)
        i += 1
      }

      if (m_damageRedraw) m_clip.reset()
      checkItemBoundsChanged(m_rclip)
    }
  }
  */
}