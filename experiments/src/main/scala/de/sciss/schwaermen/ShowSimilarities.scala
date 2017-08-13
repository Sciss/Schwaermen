package de.sciss.schwaermen

import java.awt.Color
import java.io.{DataInputStream, FileInputStream}
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

import scala.swing.Swing.{VStrut, _}
import scala.swing.event.{ButtonClicked, ValueChanged}
import scala.swing.{Action, BorderPanel, BoxPanel, Button, Component, FlowPanel, Frame, Label, Menu, MenuBar, MenuItem, Orientation, ProgressBar, ToggleButton}
import scala.collection.breakOut
import scala.concurrent.{Future, blocking}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Failure

object ShowSimilarities {
  def loadGraph(textIndices: Seq[Int], weightPow: Double = 1.0, dropAmt: Double = 0.0): List[SimEdge] = {
    val fIn = file(s"/data/temp/edges${textIndices.mkString}.bin")
    val din = new DataInputStream(new FileInputStream(fIn))
    val edgesI = try {
      val numEdges = (fIn.length() / 10 /* 8 */).toInt

      val vertices: Map[Int, Vector[Vertex]] =
        textIndices.map(i => i -> BuildSimilarities.readVertices(i).to[Vector])(breakOut)

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
        Edge(v1, v2, sim1): SimEdge
      }
    } finally {
      din.close()
    }

    val edgesIS = edgesI.sortBy(_.weight)
    val drop    = (edgesIS.size * dropAmt).toInt
    if (drop > 0) println(s"Dropping $drop (${(dropAmt * 100).toInt}%) edges")
    val edgesID = edgesIS.drop(drop)

    val minSim  = edgesID.iterator.map(_.weight).min
    val maxSim  = edgesID.iterator.map(_.weight).max
    // println(f"minSim = $minSim%g, maxSim = $maxSim%g")
    import numbers.Implicits._
    val edgesN = edgesID.map(e => e.copy(weight = e.weight.linlin(minSim, maxSim, 1.0, 0.0).pow(weightPow)))

    implicit val ord: Ordering[Vertex] = Ordering.by(_.words.head.index)
    val edges = MSTKruskal[Vertex, SimEdge](edgesN)
    edges
  }

//  private[this] val colors = Array[Int](0xFF0000, 0x00C000, 0x0000FF)
  private[this] val colors = Array[Int](0xFF0000, 0x0000FF, 0x00C000)

  final val USE_COLOR = true

  def main(args: Array[String]): Unit = {
    val edges = loadGraph(1 :: 3 :: Nil, dropAmt = 0.2)
    val vertices  = edges.flatMap(e => Seq(e.start, e.end)).toSet
    val wordMap: Map[Vertex, Visual.Word] = vertices.map { v =>
      v -> Visual.Word(v.wordsString, color = if (USE_COLOR) colors(v.textIdx - 1) else 0)
    } (breakOut)

    val wordEdges = edges.map { e =>
      Edge(wordMap(e.start), wordMap(e.end), e.weight)
    }

    onEDT {
      mkFrame(wordEdges)
    }
  }

  def mkFrame(edges: Visual.WordEdges): Unit = {
    val v = Visual()
    v.displaySize = (800, 800)

    v.text = edges
    if (USE_COLOR) {
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

    //    split.oneTouchExpandable  = true
    //    split.continuousLayout    = false
    //    split.dividerLocation     = 800
    //    split.resizeWeight        = 1.0

    val mb = new MenuBar {
      contents += new Menu("File") {
        contents += new MenuItem(new Action("Export Image...") {
          accelerator = Some(KeyStroke.getKeyStroke("ctrl S"))
          def apply(): Unit = {
            FileDialog.save(init = Some(userHome / "Documents" / "out.png")).show(None).foreach { f =>
              val pFull = new RenderImage(v,
                width   = mWidth .getNumber.intValue,
                height  = mHeight.getNumber.intValue,
                fOut = f.replaceExt("png"))
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