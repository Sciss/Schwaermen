/*
 *  AutoZoom.scala
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

import java.awt.geom.{Point2D, Rectangle2D}

import prefuse.Display
import prefuse.action.Action
import prefuse.util.GraphicsLib

class AutoZoom(main: Visual, frac: Double = 0.01, margin: Int = 20) extends Action {
  // var karlHeinz = 1.0

  def run(frac: Double): Unit = {
    val bounds = main.visualization.getBounds(Visual.GROUP_GRAPH)
    zoom(bounds)
  }

  private def zoom(bounds: Rectangle2D): Unit = {
    val d = main.display
    if (d.isTranformInProgress) return
    GraphicsLib.expand(bounds, margin + (1 / d.getScale).toInt)
    fitViewToBounds(d, bounds)
  }

  private def fitViewToBounds(display: Display, bounds: Rectangle2D): Unit = {
    val w       = display.getWidth  // * karlHeinz
    val h       = display.getHeight // * karlHeinz
    val dx      = display.getDisplayX
    val dy      = display.getDisplayY
    val cx      = bounds.getCenterX // * karlHeinz
    val cy      = bounds.getCenterY // * karlHeinz
    val wb      = math.max(cx - bounds.getMinX, bounds.getMaxX - cx)
    val hb      = math.max(cy - bounds.getMinY, bounds.getMaxY - cy)
    val scale0  = math.min(w / (2 * wb), h / (2 * hb))

    val ds      = display.getScale // * karlHeinz
    val scale1  = scale0 / ds // math.max(Visual.VIDEO_WIDTH_SQR, scale0) / ds
    val scale2  = math.min(2.0, scale1 * ds) / ds

    //    val dx      = display.getDisplayX + w * 0.5 * ds
    //    val dy      = display.getDisplayY + h * 0.5 * ds

    // val fracI   = 1.0 - frac

    // val cx1     = cx // * frac // + dx * fracI
    // val cy1     = cy // * frac // + dy * fracI
    val center  = new Point2D.Double(cx, cy)
    val scale   = math.pow(scale2, frac)

    // println(f"dx $dx%1.2f, dy $dy%1.2f, cx $cx1%1.2f, cy $cy1%1.2f; scale = $ds%1.2f")

    val ax = (w / (2 * ds) - cx) + dx / ds
    val ay = (h / (2 * ds) - cy) + dy / ds

    /*
        double sx = m_transform.getScaleX();
        double sy = m_transform.getScaleY();
        double x = p.getX(); x = (Double.isNaN(x) ? 0 : x);
        double y = p.getY(); y = (Double.isNaN(y) ? 0 : y);
        x = getWidth() /(2*sx) - x;
        y = getHeight()/(2*sy) - y;

        double dx = x-(m_transform.getTranslateX()/sx);
        double dy = y-(m_transform.getTranslateY()/sy);

     */

    // display.panToAbs(center)
    display.panAbs(ax * frac, ay * frac)
    display.zoomAbs(center, scale)
  }
}