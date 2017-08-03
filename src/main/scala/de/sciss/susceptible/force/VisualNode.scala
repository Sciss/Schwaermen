/*
 *  VisualNode.scala
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

import java.awt.Shape

import prefuse.visual.VisualItem

import scala.swing.Graphics2D

/** The common trait of all visible objects on the
  * Prefuse display.
  *
  * The next sub-type is `VisualNode` that is represented by a graph node.
  */
trait VisualData extends Disposable {
  def main: Visual

  // def touch()(implicit tx: S#Tx): Unit
}

trait VisualNode extends VisualData {
  // def pNode: PNode
  // var edges: Map[(VisualNode, VisualNode), VisualEdge]

  /** Asks the receiver to paint its GUI representation. */
  def render(g: Graphics2D, vi: VisualItem): Unit

  def getShape(x: Double, y: Double): Shape

  def name: String
}