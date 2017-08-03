package de.sciss.schwaermen

import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.confluent.TxnRandom
import de.sciss.lucre.expr.{DoubleObj, LongObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.schwaermen.BuildSimilarities.Vertex
import de.sciss.schwaermen.ExplorePaths.{EdgeMap, WEIGHT_POW, mkEdgeMap}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Action, AudioCue, AuralSystem, Proc, TimeRef, Transport, WorkspaceHandle}
import de.sciss.synth.{SynthGraph, proc}
import de.sciss.lucre.stm.TxnLike.peer

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
    val play  = VDiskIn.ar("disk")
    val dur   = "dur".ir
    val line  = Line.kr(0, 0, dur)
    Act(Done.kr(line), "done")
    Out.ar(0, play)
  }

  class State(val p: Proc[S], val offset: LongObj.Var[S], val duration: DoubleObj.Var[S],
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

      val path0     = path()
      val pathPos0  = pathPos()
      val (path1, pathPos1) = if (pathPos0 < path0.size) (path0, pathPos0) else {
        val p = newPath()
        (p, 0)
      }
      pathPos() = pathPos1 + 1

      val v = path1(pathPos1)

      val offV    = framesFromFile(v.span.start)
      val durV    = v.span.length / fileSR
      offset  ()  = offV
      duration()  = durV

      println(f"vertex ${v.quote}, offset $offV, duration $durV%g")

      t.play()
    }
  }

  def booted(aural: AuralSystem, s: Server, edgeMap: EdgeMap)
            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val vertices = edgeMap.keys.toVector

    val fileIn    = BuildSimilarities.audioFileIn
    val specIn    = AudioFile.readSpec(fileIn)
    val loc       = ArtifactLocation.newConst[S](fileIn.parent)
    val artIn     = Artifact(loc, fileIn)
    val offVar    = LongObj   .newVar[S](0L)
    val durVar    = DoubleObj .newVar[S](2.0)
    val cue       = AudioCue.Obj[S](artIn, specIn, offVar, 1.0)

    val p = Proc[S]
    import WorkspaceHandle.Implicits.dummy

    val rnd   = TxnRandom(tx.newID())
    val t     = Transport[S](aural)
    val state = new State(p, offVar, durVar, t, vertices, edgeMap, rnd)

    p.graph() = playGraph
    val pa = p.attr
    pa.put("disk", cue)
    pa.put("dur" , durVar)
    Action.registerPredef("disk.done", new DiskDone(state))
    pa.put("done", Action.predef("disk.done"))

    t.addObject(p)

    state.iterate()
  }

  val fileSR: Double = 44100.0

  def framesFromFile(n: Long): Long = (n * TimeRef.SampleRate / fileSR).toLong
}