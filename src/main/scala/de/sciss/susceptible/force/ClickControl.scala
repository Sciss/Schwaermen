/*
 *  ClickControl.scala
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

import java.awt.event.MouseEvent
import java.awt.geom.Rectangle2D

import prefuse.Display
import prefuse.controls.ControlAdapter
import prefuse.util.GraphicsLib
import prefuse.util.display.DisplayLib
import prefuse.visual.VisualItem

class ClickControl(main: Visual) extends ControlAdapter {

  override def mousePressed(e: MouseEvent): Unit = {
    if (e.isMetaDown) {
      zoomToFit(e)
    }
  }

  override def itemPressed(vi: VisualItem, e: MouseEvent): Unit = {
    if (e.isAltDown) {
      return
    }
    if (e.isMetaDown) {
      zoom(e, vi.getBounds)
    }
  }

  private def zoomToFit(e: MouseEvent): Unit = {
    val d       = getDisplay(e)
    val vis     = d.getVisualization
    val bounds  = vis.getBounds(Visual.GROUP_GRAPH)
    zoom(e, bounds)
  }

  private def zoom(e: MouseEvent, bounds: Rectangle2D): Unit = {
    val d = getDisplay(e)
    if (d.isTranformInProgress) return
    val margin    = 50 // XXX could be customized
    val duration  = 1000 // XXX could be customized
    GraphicsLib.expand(bounds, margin + (1 / d.getScale).toInt)
    DisplayLib.fitViewToBounds(d, bounds, duration)
  }

  @inline private def getDisplay(e: MouseEvent) = e.getComponent.asInstanceOf[Display]
}