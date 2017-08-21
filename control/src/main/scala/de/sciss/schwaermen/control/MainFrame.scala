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
import javax.swing.{ButtonGroup, Icon, JTable, SwingConstants}

import de.sciss.desktop.FileDialog
import de.sciss.file._
import de.sciss.osc
import de.sciss.swingplus.Table
import de.sciss.synth.SynthDef

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.Table.AutoResizeMode
import scala.swing.event.{ButtonClicked, TableRowsSelected, ValueChanged}
import scala.swing.{BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Label, Orientation, ScrollPane, Slider, Swing, ToggleButton}

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
      val xn = (x + amount * 70).toInt // 2
      while (xi < xn) {
        g.drawLine(xi, y0, xi, y1)
        xi += 2
      }
    }
  }

  private val columns: Array[Column] = Array(
    Column( 0, "Pos"    , 64,  64,  64, _.pos     , Some(RightAlignedRenderer), Some(Ordering.Int)),
    Column( 1, "Id"     , 64,  64,  64, _.dot     , Some(RightAlignedRenderer), Some(Ordering.Int)),
    Column( 2, "Version", 64, 360, 360, _.version , None, None),
    Column( 3, "Update" , 74,  74,  74, _.update  , Some(AmountRenderer)      , Some(Ordering.Double))
  )

  private object model extends AbstractTableModel {
    private[this] var _instances = Vector.empty[Status]

    def getRowCount   : Int = _instances.size
    def getColumnCount: Int = columns.length

    def instances: Vec[Status] = _instances

    override def getColumnName(colIdx: Int): String = columns(colIdx).name

    def clear(): Unit = {
      val sz = _instances.size
      if (sz > 0) {
        _instances = Vector.empty
        fireTableRowsDeleted(0, sz - 1)
      }
    }

    def += (status: Status): Unit = {
      val row = _instances.size
      _instances :+= status
      fireTableRowsInserted(row, row)
    }

    def -= (status: Status): Unit = {
      val row = _instances.indexOf(status)
      if (row < 0) throw new IllegalArgumentException(s"Status $status was not in table")
      _instances = _instances.patch(row, Nil, 1)
      fireTableRowsDeleted(row, row)
    }

    def update(status: Status): Unit = {
      val row = _instances.indexWhere(_.dot == status.dot)
      if (row < 0) throw new IllegalArgumentException(s"Dot ${status.dot} was not occupied")
      _instances = _instances.updated(row, status)
      fireTableRowsUpdated(row, row)
    }

    def getValueAt(rowIdx: Int, colIdx: Int): AnyRef = {
      val status  = _instances(rowIdx)
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
//    res.selection.elementMode = ...
    res.reactions += {
      case TableRowsSelected(_, _, false) => selectedChanged()
    }

    res
  }

  private def selection: Vec[Status] = {
    val xs      = model.instances
    val rows    = table.selection.rows
    val res     = rows.iterator.map { vi =>
      val mi = table.viewToModelRow(vi)
      xs(mi)
    } .toIndexedSeq
    res
  }

  private[this] val ggRefresh = Button("Refresh List") {
    // XXX TODO --- restart time-out timer that removes instances which do not respond
//    model.clear()
    c ! Network.OscQueryVersion
  }

  private[this] var lastUpdate = Option.empty[File]

  private[this] val ggUpdate = Button("Update Software...") {
    val instances = selection
    if (instances.nonEmpty) {
      val isSound = instances.head.version.contains("sound")
      val dir = lastUpdate.flatMap(_.parentOption).getOrElse {
        userHome / "Documents" / "devel" / "Schwaermen" / (if (isSound) "sound" else "video") / "target"
      }
      val candidates  = dir.children(_.ext == "deb")
      // this also works for `-SNAPSHOT_all.deb` vs. `_all.deb`
      val sorted      = candidates.sorted(File.NameOrdering)
      val init        = sorted.lastOption.orElse(if (dir.isDirectory) Some(dir) else None)
      val dlg         = FileDialog.open(init = init, title = "Select .deb file")
      dlg.show(None).foreach { debFile =>
        lastUpdate = Some(debFile)
        c.beginUpdates(debFile, instances)
      }
    }
  }

  private[this] val ggReboot = Button("Reboot") {
    selection.foreach { instance =>
      c.tx.send(Network.OscReboot, instance.socketAddress)
    }
  }

  private[this] val ggShutdown = Button("Shutdown") {
    selection.foreach { instance =>
      c.tx.send(Network.OscShutdown, instance.socketAddress)
    }
  }

  private[this] val ggTestPins = Button("Test Pins") {
    selection.foreach { instance =>
      c.tx.send(osc.Message("/test-pin-mode"), instance.socketAddress)
    }
  }

  private[this] val ggTestPath = Button("Test Path") {
    selection.foreach { instance =>
      c.tx.send(osc.Message("/test-path-finder"), instance.socketAddress)
    }
  }

  private def ToggleButton(title: String, init: Boolean)(fun: Boolean => Unit): ToggleButton =
    new ToggleButton(title) {
      if (init) selected = true
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => fun(selected)
      }
    }

  private[this] val ggSoundOff    = new ToggleButton("Off"  )
  private[this] val ggSoundPing   = new ToggleButton("Ping" )
  private[this] val ggSoundNoise  = new ToggleButton("Noise")
//  private[this] val ggSoundNegatum= new ToggleButton("Negatum")
  private[this] val gSound = new ButtonGroup
  gSound.add(ggSoundOff     .peer)
  gSound.add(ggSoundPing    .peer)
  gSound.add(ggSoundNoise   .peer)
//  gSound.add(ggSoundNegatum .peer)

  private[this] val ggBees = ToggleButton("Bees", init = true) { onOff =>
    selection.foreach { instance =>
      c.tx.send(osc.Message("/bees", onOff), instance.socketAddress)
    }
  }

  private def selectedChanged(): Unit = {
    val hasSelection    = selection.nonEmpty
    ggUpdate  .enabled  = hasSelection
    ggReboot  .enabled  = hasSelection
    ggShutdown.enabled  = hasSelection
  }

  selectedChanged()

//  private def graphBubbles(): Unit = {
//    import de.sciss.synth._
//    import Ops.stringToControl
//    import ugen._
//    val o   = LFSaw.kr(Seq(8, 7.23)).madd(3, 80)
//    val f   = LFSaw.kr(0.4).madd(24, o)
//    val s   = SinOsc.ar(f.midicps) * 0.24
//    val sig = Limiter.ar(CombN.ar(s, 0.2, 0.2, 4))
//    Out.ar("bus".kr, Mix.mono(sig))
//  }

  private[this] val ggAmp = new Slider {
    min   = -60
    max   =   0
    value = -12
    listenTo(this)
    reactions += {
      case ValueChanged(_) =>
        println("TODO - set master volume")
    }
  }

  private[this] val pButtons1 = new FlowPanel(ggRefresh, ggUpdate, ggReboot, ggShutdown, ggTestPins, ggTestPath, ggBees)
  private[this] val pButtons2 = new FlowPanel(
    new Label("Sound:"), ggSoundOff, ggSoundPing, ggSoundNoise /* , ggSoundNegatum */)
  private[this] val pChannels = new FlowPanel(Seq.tabulate(12) { ch =>
    Button((ch + 1).toString) {
      selection.foreach { instance =>
//        if (ggSoundNegatum.selected) {
//          val tpe = 2
//          import de.sciss.numbers.Implicits._
//          val amp  = ggAmp.value.dbamp
//          val df = SynthDef("test") {
////            NegatumGraphs.g1_51_4456(amp) // graphBubbles()
//            NegatumGraphs.g1_51_4533(amp) // graphBubbles()
//          }
//          val rest = df.recvMsg.bytes
//          c.tx.send(osc.Message("/test-channel", ch, tpe, rest), instance.socketAddress)
//        } else {
          val tpe = if (ggSoundPing.selected) 0 else if (ggSoundNoise.selected) 1 else -1
          c.tx.send(osc.Message("/test-channel", ch, tpe), instance.socketAddress)
//        }
      }
    }
  }: _*)

  private[this] val pBottom = new BoxPanel(Orientation.Vertical) {
    contents += pButtons1
    contents += pButtons2
    contents += pChannels
    contents += new FlowPanel(new Label("Main Vol."), ggAmp)
  }

  private[this] val component: Component = {
    val scroll = new ScrollPane(table)
    scroll.peer.putClientProperty("styleId", "undecorated")
    scroll.preferredSize = {
      val d = scroll.preferredSize
      d.width = math.min(540, table.preferredSize.width)
      d
    }
    new BorderPanel {
      add(scroll , BorderPanel.Position.Center )
      add(pBottom, BorderPanel.Position.South  )
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