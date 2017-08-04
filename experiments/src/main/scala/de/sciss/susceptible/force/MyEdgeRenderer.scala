/*
 *  MyEdgeRenderer.scala
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

import prefuse.Constants
import prefuse.render.EdgeRenderer

class MyEdgeRenderer
  extends EdgeRenderer(Constants.EDGE_TYPE_LINE, Constants.EDGE_ARROW_NONE) {

  // private val dot = new Ellipse2D.Float(-m_arrowWidth * 0.5f, -m_arrowHeight, m_arrowWidth, m_arrowHeight)

  // setArrowHeadSize(5, 5)

  //  override def setArrowHeadSize(width: Int, height: Int): Unit = {
  //    super.setArrowHeadSize(width, height)
  //    dot.setFrame(-width * 0.5f, -height, width, height)
  //  }

//  override def drawShape(g: Graphics2D, item: VisualItem, shape: Shape): Unit = ()

  //  override def getRawShape(item: VisualItem): Shape = {
  //    val edge  = item.asInstanceOf[EdgeItem]
  //    val item1 = edge.getSourceItem
  //    val item2 = edge.getTargetItem
  //
  //    val tpe   = m_edgeType
  //
  //    EdgeRenderer.getAlignedPoint(m_tmpPoints(0), item1.getBounds, m_xAlign1, m_yAlign1)
  //    EdgeRenderer.getAlignedPoint(m_tmpPoints(1), item2.getBounds, m_xAlign2, m_yAlign2)
  //    m_curWidth = (m_width * getLineWidth(item)).toFloat
  //
  //    // create the arrow head, if needed
  //    val e: EdgeItem = item.asInstanceOf[EdgeItem]
  //
  //    if (e.isDirected && m_edgeArrow != Constants.EDGE_ARROW_NONE) {
  //      // get starting and ending edge endpoints
  //      val forward = m_edgeArrow == Constants.EDGE_ARROW_FORWARD
  //
  //      val start = m_tmpPoints(if (forward) 0 else 1)
  //      var end   = m_tmpPoints(if (forward) 1 else 0)
  //      val dest  = if (forward) e.getTargetItem else e.getSourceItem
  //      val i     = GraphicsLib.intersectLineRectangle(start, end, dest.getBounds, m_isctPoints)
  //      if (i > 0) end = m_isctPoints(0)
  //      val at: AffineTransform = getArrowTrans(start, end, m_curWidth)
  //      m_curArrow  = at.createTransformedShape(dot)
  //      val lineEnd = m_tmpPoints(if (forward) 1 else 0)
  //      lineEnd.setLocation(0, -m_arrowHeight)
  //      at.transform(lineEnd, lineEnd)
  //    } else {
  //      m_curArrow = null
  //    }
  //
  //    // create the edge shape
  //    var shape: Shape = null
  //    val n1x: Double = m_tmpPoints(0).getX
  //    val n1y: Double = m_tmpPoints(0).getY
  //    val n2x: Double = m_tmpPoints(1).getX
  //    val n2y: Double = m_tmpPoints(1).getY
  //    tpe match {
  //      case Constants.EDGE_TYPE_LINE =>
  //        m_line.setLine(n1x, n1y, n2x, n2y)
  //        shape = m_line
  //
  //      case Constants.EDGE_TYPE_CURVE =>
  //        getCurveControlPoints(edge, m_ctrlPoints, n1x, n1y, n2x, n2y)
  //        m_cubic.setCurve(n1x, n1y, m_ctrlPoints(0).getX, m_ctrlPoints(0).getY, m_ctrlPoints(1).getX, m_ctrlPoints(1).getY, n2x, n2y)
  //        shape = m_cubic
  //
  //      case _ =>
  //        throw new IllegalStateException("Unknown edge type")
  //    }
  //
  //    // return the edge shape
  //    shape
  //  }
}