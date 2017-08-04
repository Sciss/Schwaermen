/*
 *  MyForceDirectedLayout.scala
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

import prefuse.action.layout.graph.ForceDirectedLayout
import prefuse.util.force.SpringForce
import prefuse.visual.EdgeItem

class MyForceDirectedLayout(main: Visual, spring: MySpringForce)
  extends ForceDirectedLayout(Visual.GROUP_GRAPH) {

  override def getSpringLength(e: EdgeItem): Float = {
//    val nSrc = e.getSourceItem
//    val nDst = e.getTargetItem

    val f = e.getDouble(Visual.COL_WEIGHT).toFloat
//    println(f)
    val scale = spring.getParameter(SpringForce   .SPRING_LENGTH)
    val pow   = spring.getParameter(MySpringForce .POW)
    math.pow(f, pow).toFloat * scale

//    (nSrc.get(Visual.COL_MUTA), nDst.get(Visual.COL_MUTA)) match {
//      case (vSrc: VisualVertex, vDst: VisualVertex) =>
//        if (vSrc.lineRef eq vDst.lineRef) {
//          val res = vSrc.advance
//          // println(s"ADVANCE = $res")
//          res
//        } else -1
//      case _ =>
//        // println("Oh noes!")
//        -1f
//    }
  }

  // this is used to mark horizontal springs (using coefficient zero)
  override def getSpringCoefficient(e: EdgeItem): Float = {
    val f = e.getDouble(Visual.COL_WEIGHT).toFloat
    val scale = spring.getParameter(SpringForce   .SPRING_COEFF)
    f * scale

//    val nSrc = e.getSourceItem
//    val nDst = e.getTargetItem
//
//    (nSrc.get(Visual.COL_MUTA), nDst.get(Visual.COL_MUTA)) match {
//      case (vSrc: VisualVertex, vDst: VisualVertex) =>
//        if (vSrc.wordRef eq vDst.wordRef) 0f else
//        if (vSrc.lineRef eq vDst.lineRef) 1f else 2f
//      case _ =>
//        // println("Oh noes!")
//        -1f
//    }
  }

  private var _counter = 0

  override def run(frac: Double): Unit = {
    _counter += 1
    super.run(frac)
  }

  def counter: Int = _counter
}