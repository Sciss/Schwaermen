/*
 *  VisualVertex.scala
 *  (Susceptible)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.susceptible.force

import prefuse.data.{Node => PNode}
import prefuse.visual.VisualItem

import scala.swing.Graphics2D

object VisualVertex {
  def apply(main: Visual, word: String, color: Int = 0): VisualVertex =
    new Impl(main, word = word, color = color | 0xFF000000)

  private final class Impl(val main: Visual, var word: String, val color: Int)
    extends VisualVertex with VisualVertexImpl {

    protected def renderDetail(g: Graphics2D, vi: VisualItem): Unit = {
      drawLabel(g, vi, /* diam * vi.getSize.toFloat * 0.5f, */ name)
    }

    protected def boundsResized(): Unit = ()

    def name: String = word

    private var _pNode: PNode = _

    def pNode: PNode = _pNode

    def init(): Unit = {
      _pNode = mkPNode()
      // println(s"ADD NODE ${_pNode.hashCode().toHexString} - $character")
    }

    def dispose(): Unit = {
      if (_pNode.isValid) {
        // println(s"REMOVE NODE ${_pNode.hashCode().toHexString} - $character")
        main.graph.removeNode(_pNode)
      }
      // else println(s"NOT VALID: ${_pNode.hashCode().toHexString}")
    }

    init()
  }
}
sealed trait VisualVertex extends VisualNode {
  def word: String

  def advance: Int

  def pNode: PNode

//  def wordRef: AnyRef
//  var lineRef: AnyRef
}