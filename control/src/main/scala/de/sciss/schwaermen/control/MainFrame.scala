/*
 *  MainFrame.scala
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
package control

import java.util.Comparator
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableCellRenderer, TableRowSorter}
import javax.swing.{Icon, JTable, SwingConstants}

import de.sciss.swingplus.Table

import scala.swing.Table.AutoResizeMode
import scala.swing.{BorderPanel, Button, Component, FlowPanel, Frame, ScrollPane, Swing}

class MainFrame(c: OSCClient) {
  private case class Column(idx: Int, name: String, minWidth: Int, prefWidth: Int, maxWidth: Int,
                            extract: Status => Any, cellRenderer: Option[TableCellRenderer] = None,
                            sorter: Option[Comparator[_]] = None, headerRenderer: Option[TableCellRenderer] = None)

  private[this] val RightAlignedRenderer = {
    val res = new DefaultTableCellRenderer
    res.setHorizontalAlignment(SwingConstants.TRAILING)
    res
  }

  private object AmountRenderer extends DefaultTableCellRenderer with Icon {
    setIcon(this)

    private[this] var amount = 0.0

    override def setValue(value: AnyRef): Unit = {
      amount = if (value == null) 0.0 else value match {
        case i: java.lang.Double  => i.doubleValue()
        case _                    => 0.0
      }
    }

    def getIconWidth  = 21
    def getIconHeight = 16

    def paintIcon(c: java.awt.Component, g: java.awt.Graphics, x: Int, y: Int): Unit = {
      g.setColor(getForeground)
      var xi = x + 1
      val y0 = y + 2
      val y1 = y + 14
      val xn = (x + amount).toInt // 2
      while (xi < xn) {
        g.drawLine(xi, y0, xi, y1)
        xi += 2
      }
    }
  }

  private val columns: Array[Column] = Array(
    Column( 0, "Pos"    , 64,  64,  64, _.pos     , Some(RightAlignedRenderer), Some(Ordering.Int)),
    Column( 1, "Id"     , 64,  64,  64, _.dot     , Some(RightAlignedRenderer), Some(Ordering.Int)),
    Column( 2, "Version", 64, 320, 320, _.version , None, None),
    Column( 3, "Update" , 60,  60,  60, _.update  , Some(AmountRenderer)      , Some(Ordering.Double))
  )

  private object model extends AbstractTableModel {
    private[this] var instances = Vector.empty[Status]

    def getRowCount   : Int = instances.size
    def getColumnCount: Int = columns.length

    override def getColumnName(colIdx: Int): String = columns(colIdx).name

    def += (status: Status): Unit = {
      val row = instances.size
      instances :+= status
      fireTableRowsInserted(row, row)
    }

    def -= (status: Status): Unit = {
      val row = instances.indexOf(status)
      if (row < 0) throw new IllegalArgumentException(s"Status $status was not in table")
      instances = instances.patch(row, Nil, 1)
      fireTableRowsDeleted(row, row)
    }

    def update(status: Status): Unit = {
      val row = instances.indexWhere(_.dot == status.dot)
      if (row < 0) throw new IllegalArgumentException(s"Dot ${status.dot} was not occupied")
      instances = instances.updated(row, status)
      fireTableRowsUpdated(row, row)
    }

    def getValueAt(rowIdx: Int, colIdx: Int): AnyRef = {
      val status  = instances(rowIdx)
      val col     = columns(colIdx)
      col.extract(status).asInstanceOf[AnyRef]
    }
  }

  c.addListener {
    case OSCClient.Added  (status) => Swing.onEDT(model += status)
    case OSCClient.Removed(status) => Swing.onEDT(model -= status)
    case OSCClient.Changed(status) => Swing.onEDT(model.update(status))
  }

  c.instances.foreach(model += _)

  private[this] val table: Table = {
    val res = new Table {
      // https://github.com/scala/scala-swing/issues/47
      override lazy val peer: JTable = new JTable with SuperMixin
    }
    res.model   = model
    val resJ    = res.peer
    val cm      = resJ.getColumnModel
    val sorter  = new TableRowSorter(model)
    columns.foreach { col =>
      val tc = cm.getColumn(col.idx)
      col.sorter.foreach(sorter.setComparator(col.idx, _))
      tc.setMinWidth      (col.minWidth )
      tc.setMaxWidth      (col.maxWidth )
      tc.setPreferredWidth(col.prefWidth)
      col.cellRenderer  .foreach(tc.setCellRenderer  )
      col.headerRenderer.foreach(tc.setHeaderRenderer)
    }
    // cm.setColumnMargin(6)
    resJ.setRowSorter(sorter)
    // cf. http://stackoverflow.com/questions/5968355/horizontal-bar-on-jscrollpane/5970400
    res.autoResizeMode = AutoResizeMode.Off
    // resJ.setPreferredScrollableViewportSize(resJ.getPreferredSize)

    res.listenTo(res.selection)
    res.selection.elementMode
//    res.reactions += {
//      case TableRowsSelected(_, _, false) =>
//        dispatch(SoundTableView.Selection(selection))
//    }

    res
  }

  private[this] val ggRefresh = Button("Refresh") {
    c ! Network.oscQueryVersion
  }

  private[this] val pButtons = new FlowPanel(ggRefresh)

  private[this] val component: Component = {
    val scroll = new ScrollPane(table)
    scroll.peer.putClientProperty("styleId", "undecorated")
    scroll.preferredSize = {
      val d = scroll.preferredSize
      d.width = math.min(512, table.preferredSize.width)
      d
    }
    new BorderPanel {
      add(scroll  , BorderPanel.Position.Center )
      add(pButtons, BorderPanel.Position.South  )
    }
  }


  /* private[this] val frame = */ new Frame {
    override def closeOperation(): Unit =
      sys.exit(0)

    title = "Schwärmen Control"
    contents = component
    pack().centerOnScreen()
    open()
  }
}