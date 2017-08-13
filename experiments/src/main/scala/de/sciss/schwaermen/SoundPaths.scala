package de.sciss.schwaermen

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.expr.{DoubleObj, LongObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.schwaermen.BuildSimilarities.Vertex
import de.sciss.schwaermen.ExplorePaths.{EdgeMap, WEIGHT_POW, mkEdgeMap}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Action, AudioCue, AuralSystem, Proc, TimeRef, Transport, WorkspaceHandle}
import de.sciss.synth.{Curve, SynthGraph, proc}

import scala.Predef.{any2stringadd => _, _}
import scala.concurrent.stm.Ref

object SoundPaths {
  type S = InMemory

  def main(args: Array[String]): Unit = {
    val edges     = ShowSimilarities.loadGraph(weightPow = WEIGHT_POW)
    val map       = mkEdgeMap(edges)

    val system    = InMemory()
    val aural     = AuralSystem()
    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          booted(aural, s, map)(system.wrap(tx.peer), system)
        }

        def auralStopped()(implicit tx: Txn): Unit = ()
      })
      aural.start()
    }
  }

  class DiskDone(state: State) extends Action.Body {
    def apply[T <: Sys[T]](universe: Action.Universe[T])(implicit tx: T#Tx): Unit = {
      state.iterate()(tx.asInstanceOf[S#Tx])
    }
  }

  val playGraph: SynthGraph = SynthGraph {
    import de.sciss.synth.ugen.{VDiskIn => _, _}
    import proc.graph.{Action => Act, _}
    import Ops._
    val disk    = VDiskIn.ar("disk")
    val dur     = "dur"     .ir
    val fadeIn  = "fade-in" .ir
    val fadeOut = "fade-out".ir
//    val line    = Line.kr(0, 0, dur)
    val env     = Env.linen(attack = fadeIn, sustain = dur - (fadeIn + fadeOut), release = fadeOut, curve = Curve.sine)
    val eg      = EnvGen.ar(env)
    Act(Done.kr(eg), "done")
    val sig     = disk * eg
    Out.ar(0, sig)
  }

  class State(val p: Proc[S], val offset: LongObj.Var[S], val duration: DoubleObj.Var[S],
              val fadeIn: DoubleObj.Var[S], val fadeOut: DoubleObj.Var[S],
              val t: Transport[S], val vertices: Vector[Vertex], val edgeMap: EdgeMap,
              val rnd: TxnRandom[S#Tx]) {

    val path: Ref[Vector[Vertex]] = Ref(Vector.empty)

    def newPath()(implicit tx: S#Tx): Vector[Vertex] = {
      val startIdx = rnd.nextInt(vertices.size)
      val stopIdx  = {
        val x = rnd.nextInt(vertices.size - 1)
        if (x < startIdx) x else x + 1
      }

      val v1 = vertices(startIdx)
      val v2 = vertices(stopIdx )

      val seq = ExplorePaths.calcPath(v1, v2, edgeMap).toVector
      path() = seq
      seq
    }

    val pathPos = Ref(0)

    def iterate()(implicit tx: S#Tx): Unit = {
      t.stop()
      t.seek(0L)

      val path0: Seq[Vertex] = path()
      val pathPos0: Int = pathPos()
      val (path1, pathPos1) = if (pathPos0 < path0.size) (path0, pathPos0) else {
        val p = newPath()
        (p, 0)
      }
      pathPos() = pathPos1 + 1

      val v: Vertex = path1(pathPos1)

      val offV    = framesFromFile(v.span.start)
      val durV    = v.span.length / fileSR
      val fdInV   = v.words.head.fadeIn  * 0.5
      val fdOutV  = v.words.last.fadeOut * 0.5
      offset  ()  = offV
      duration()  = durV
      fadeIn  ()  = fdInV
      fadeIn  ()  = fdOutV

      println(f"vertex ${v.quote}, offset $offV, duration $durV%g, fade-in $fdInV%g, fade-out $fdOutV%g")

      t.play()
    }
  }

  def booted(aural: AuralSystem, s: Server, edgeMap: EdgeMap)
            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val vertices = edgeMap.keys.toVector

    val fileIn      = BuildSimilarities.audioFileIn
    val specIn      = AudioFile.readSpec(fileIn)
    val loc         = ArtifactLocation.newConst[S](fileIn.parent)
    val artIn       = Artifact(loc, fileIn)
    val offVar      = LongObj   .newVar[S](0L)
    val durVar      = DoubleObj .newVar[S](2.0)
    val fadeInVar   = DoubleObj .newVar[S](0.0)
    val fadeOutVar  = DoubleObj .newVar[S](0.0)
    val cue         = AudioCue.Obj[S](artIn, specIn, offVar, 1.0)

    val p = Proc[S]
    import WorkspaceHandle.Implicits.dummy

    val rnd   = TxnRandom(tx.newID())
    val t     = Transport[S](aural)
    val state = new State(p = p, offset = offVar, duration = durVar, fadeIn = fadeInVar, fadeOut = fadeOutVar,
      t = t, vertices = vertices, edgeMap = edgeMap, rnd = rnd)

    p.graph() = playGraph
    val pa = p.attr
    pa.put("disk"     , cue)
    pa.put("dur"      , durVar)
    pa.put("fade-in"  , fadeInVar)
    pa.put("fade-out" , fadeOutVar)
    Action.registerPredef("disk.done", new DiskDone(state))
    pa.put("done", Action.predef("disk.done"))

    t.addObject(p)

    state.iterate()
  }

  val fileSR: Double = 44100.0

  def framesFromFile(n: Long): Long = (n * TimeRef.SampleRate / fileSR).toLong
}