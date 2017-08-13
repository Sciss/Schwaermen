package de.sciss.schwaermen

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
  def loadGraph(weightPow: Double = 1.0): List[SimEdge] = {
    val fIn = file("/data/temp/edges.bin")
    val din = new DataInputStream(new FileInputStream(fIn))
    val edgesI = try {
      val numEdges = (fIn.length() / 8).toInt

      def getVertex(i: Int): Vertex =
        BuildSimilarities.readVertices(1).find(_.index == i).getOrElse(sys.error(s"Vertex $i not found"))

      List.fill(numEdges) {
        val v1Index = din.readShort()
        val v2Index = din.readShort()
        val sim0    = din.readFloat().toDouble
        val v1      = getVertex(v1Index)
        val v2      = getVertex(v2Index)
        val sim1    = sim0 // math.pow(sim0, 2)
        Edge(v1, v2, sim1): SimEdge
      }
    } finally {
      din.close()
    }

    val minSim  = edgesI.iterator.map(_.weight).min
    val maxSim  = edgesI.iterator.map(_.weight).max
    // println(f"minSim = $minSim%g, maxSim = $maxSim%g")
    import numbers.Implicits._
    val edgesN = edgesI.map(e => e.copy(weight = e.weight.linlin(minSim, maxSim, 1.0, 0.0).pow(weightPow)))

    implicit val ord: Ordering[Vertex] = Ordering.by(_.words.head.index)
    val edges = MSTKruskal[Vertex, SimEdge](edgesN)
    edges
  }

  def main(args: Array[String]): Unit = {
    val edges = loadGraph()
    val vertices  = edges.flatMap(e => Seq(e.start, e.end)).toSet
    val wordMap: Map[Vertex, Visual.Word] = vertices.map(v => v -> Visual.Word(v.wordsString))(breakOut)

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