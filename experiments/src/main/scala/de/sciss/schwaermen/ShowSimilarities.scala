/*
 *  ShowSimilarities.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen

import java.awt.Color
import java.io.DataInputStream
import javax.swing.{KeyStroke, SpinnerNumberModel}

import de.sciss.desktop.{FileDialog, OptionPane}
import de.sciss.file._
import de.sciss.numbers
import de.sciss.processor.Processor
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import de.sciss.susceptible.force.Visual
import de.sciss.swingplus.Spinner
import prefuse.util.ui.JForcePanel

import scala.collection.breakOut
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, blocking}
import scala.swing.Swing.{VStrut, _}
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Label, Menu, MenuBar, MenuItem, Orientation, ProgressBar, ToggleButton}
import scala.util.Failure

object ShowSimilarities {
  def loadGraph(textIndices: Seq[Int]): List[SimEdge] = {
    val is1         = textIndices == List(1)
    val is3         = textIndices == List(3)
    val allIndices  = if (is1 || is3) 1 :: 3 :: Nil else textIndices
    val fileId      = allIndices.mkString

    val rsrcName    = s"/edges$fileId.bin"
    val in          = getClass.getResourceAsStream(rsrcName)
    require(in != null, s"Resource '$rsrcName' not found")
    val din   = new DataInputStream(in)
    val edgesI = try {
      val numEdges = in.available() / 10

      val vertices: Map[Int, Vector[Vertex]] =
        allIndices.map(i => i -> BuildSimilarities.readVertices(i).to[Vector])(breakOut)

      def getVertex(t: Int, i: Int): Vertex =
        vertices(t).find(_.index == i).getOrElse(sys.error(s"Vertex $i not found"))

      List.fill(numEdges) {
        val v1T     = din.readByte()
        val v1Index = din.readShort()
        val v2T     = din.readByte()
        val v2Index = din.readShort()
        val sim0    = din.readFloat().toDouble
        val v1      = getVertex(v1T, v1Index)
        val v2      = getVertex(v2T, v2Index)
        val sim1    = sim0 // math.pow(sim0, 2)
        if (Vertex.Ord.lt(v1, v2))
          Edge(v1, v2)(sim1): SimEdge
        else
          Edge(v2, v1)(sim1): SimEdge
      }
    } finally {
      din.close()
    }

    val res = if (is1) {
      edgesI.filter { e =>
        e.start.textIdx == 1 && e.end.textIdx == 1
      }
    } else if (is3) {
      edgesI.filter { e =>
        e.start.textIdx == 3 && e.end.textIdx == 3
      }

    } else edgesI

    res
  }

  /** Loads a text graph and returns its minimum-spanning-tree.
    * Edges are constructed such that 'smaller' vertices (using `Vertex.Ord`) are
    * the `start` position and 'larger' vertices are the `end` position.
    */
  def loadAndSortGraph(textIndices: Seq[Int], weightPow: Double = 1.0, dropAmt: Double = 0.0,
                       mst: Boolean = true): List[SimEdge] = {
    val edgesI  = loadGraph(textIndices)
    val edgesIS = edgesI.sortBy(_.weight)
    val drop    = (edgesIS.size * dropAmt).toInt
    if (drop > 0) println(s"Dropping $drop (${(dropAmt * 100).toInt}%) edges")
    val edgesID = edgesIS.drop(drop)

    val minSim  = edgesID.iterator.map(_.weight).min
    val maxSim  = edgesID.iterator.map(_.weight).max
    // println(f"minSim = $minSim%g, maxSim = $maxSim%g")
    import numbers.Implicits._
    val edgesN = edgesID.map(e => e.updateWeight(e.weight.linlin(minSim, maxSim, 1.0, 0.0).pow(weightPow)))

//    implicit val ord: Ordering[Vertex] = Ordering.by(_.words.head.index)
    val edges = if (mst) MSTKruskal[Vertex, SimEdge](edgesN) else edgesN
    edges
  }

//  def mkMinimumSpanningTree(in: List[SimEdge]): List[SimEdge] =
//    MSTKruskal[Vertex, SimEdge](in)

//  private[this] val colors = Array[Int](0xFF0000, 0x00C000, 0x0000FF)
  private[this] val colors = Array[Int](0xFF0000, 0x0000FF, 0x00C000)

  final case class Config(textIndices: List[Int] = Nil, dropAmt: Double = 0.0,
                          displayWidth: Int = 800, displayHeight: Int = 800)

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("ShowSimilarities") {
      opt[Seq[Int]]('i', "indices")
        .required()
        .text("Text indices (one or two comma separated numbers between 1 and 3)")
        .action { (v, c) => c.copy(textIndices = v.toList) }

      opt[Double]('d', "drop")
        .text("Drop amount in percent (0 until 100)")
        .validate(v => if (v >= 0 && v < 100) success else failure("Must be >= 0 and < 100"))
        .action { (v, c) => c.copy(dropAmt = v * 100) }

      opt[Int]('w', "width")
        .text(s"Display view width in pixels (default: ${default.displayWidth})")
        .action { (v, c) => c.copy(displayWidth = v) }

      opt[Int]('h', "height")
        .text(s"Display view height in pixels (default: ${default.displayHeight})")
        .action { (v, c) => c.copy(displayHeight = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    val edges     = loadAndSortGraph(config.textIndices, dropAmt = config.dropAmt)
    val vertices  = edges.flatMap(e => Seq(e.start, e.end)).toSet
    val useColor  = config.textIndices.size > 1
    val wordMap: Map[Vertex, Visual.Word] = vertices.map { v =>
      v -> Visual.Word(v.wordsString, color = if (useColor) colors(v.textIdx - 1) else 0xFFFFFF)
    } (breakOut)

    val wordEdges = edges.map { e =>
      Edge(wordMap(e.start), wordMap(e.end))(e.weight)
    }

    onEDT {
      mkFrame(wordEdges, config, useColor = useColor)
    }
  }

  def mkFrame(edges: Visual.WordEdges, config: Config, useColor: Boolean): Unit = {
    val v = Visual()
    v.displaySize = (config.displayWidth, config.displayHeight)

    v.text = edges
    if (useColor) {
      v.edgeColor = Color.gray
    }

    lazy val ggAutoZoom: ToggleButton = new ToggleButton("Zoom") {
      selected = true
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          v.autoZoom = selected
      }
    }

    lazy val ggRunAnim: ToggleButton = new ToggleButton("Anim") {
      selected = true
      listenTo(this)
      reactions += {
        case ButtonClicked(_) =>
          v.runAnimation = selected
      }
    }

    val mOutline = new SpinnerNumberModel(0, 0, 10, 1)
    lazy val ggOutline: Spinner = new Spinner(mOutline) {
      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          v.textOutline = mOutline.getNumber.intValue()
      }
    }

    val mWidth = new SpinnerNumberModel(8192, 16, 16384, 1)
    val ggWidth: Spinner = new Spinner(mWidth)

    val mHeight = new SpinnerNumberModel(8192, 16, 16384, 1)
    val ggHeight: Spinner = new Spinner(mHeight)

    lazy val pBottom: Component = new BoxPanel(Orientation.Vertical) {
      contents += new FlowPanel(ggAutoZoom, ggRunAnim, new Label("Outline:"),
        ggOutline, new Label("Width:"), ggWidth, new Label("Height:"), ggHeight)
    }
    lazy val pRight: BoxPanel = new BoxPanel(Orientation.Vertical) {
      contents += VStrut(16)  // will be replaced
      //      contents += cfgView.component
      //      contents += ggText
    }

    // stupidly, it doesn't listen for model changes
    def mkForcePanel(): Unit = {
      val fSim    = v.forceSimulator
      val fPanel  = new JForcePanel(fSim)
      fPanel.setBackground(null)
      pRight.contents.update(0, Component.wrap(fPanel))
    }

    mkForcePanel()

    val split = new BorderPanel {
      add(v.component, BorderPanel.Position.Center )
      add(pBottom    , BorderPanel.Position.South )
    }

    val mb = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Export Image...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl S"))
          def apply(): Unit = {
            val dlg = FileDialog.save(init = Some(userHome / "Documents" / "out.png"))
            dlg.title = "Choose output png or pdf file"
            dlg.show(None).foreach { f =>
              val pFull = new RenderImage(v,
                width   = mWidth .getNumber.intValue,
                height  = mHeight.getNumber.intValue,
                fOut = if (f.extL == "pdf") f else f.replaceExt("png"))
              pFull.start()
              val futTail = pFull
              mkProgressDialog("Exporting...", pFull, futTail)
            }
          }
        })
      }
    }

    new Frame {
      title     = "Text"
      contents  = new BorderPanel {
        add(split   , BorderPanel.Position.Center)
        // add(pBottom , BorderPanel.Position.South)
        add(pRight  , BorderPanel.Position.East)
      }
      menuBar = mb
      pack().centerOnScreen()

      open()

      override def closeOperation(): Unit = {
        try {
          // v.algorithm.system.close()
        } finally {
          sys.exit(0)
        }
      }
    }

    v.display.panAbs(400, 400)
    v.runAnimation = true
  }

  private final class RenderImage(v: Visual, width: Int, height: Int, fOut: File)
    extends ProcessorImpl[Unit, Processor[Unit]] with Processor[Unit] {

    protected def body(): Unit = blocking {
      //      if (!fOut.exists()) {
      if (fOut.extL == "pdf")
        v.saveFrameAsPDF(fOut, width = width, height = height)
      else
        v.saveFrameAsPNG(fOut, width = width, height = height)
      //      }
      progress = 1.0
    }
  }

  def mkProgressDialog(title: String, p: Processor[Any], tail: Future[Any]): Unit = {
    val ggProg  = new ProgressBar
    val ggAbort = new Button("Abort")
    val opt     = OptionPane(message = ggProg, messageType = OptionPane.Message.Plain, entries = Seq(ggAbort))

    val optPeer = opt.peer
    val dlg = optPeer.createDialog(title)
    ggAbort.listenTo(ggAbort)
    ggAbort.reactions += {
      case ButtonClicked(_) =>
        p.abort()
    }
    tail.onComplete(_ => onEDT(dlg.dispose()))
    tail.onComplete {
      case Failure(Processor.Aborted()) =>
      case Failure(ex) => ex.printStackTrace()
      case _ =>
    }
    p.addListener {
      case prog @ Processor.Progress(_, _) => onEDT(ggProg.value = prog.toInt)
    }
    dlg.setVisible(true)
  }
}