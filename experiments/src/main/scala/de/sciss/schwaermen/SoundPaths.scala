/*
 *  SoundPaths.scala
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

import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, LongObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Sys, TxnRandom}
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.synth.{InMemory, Server, Txn}
import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}
import de.sciss.schwaermen.ExplorePaths.{EdgeMap, WEIGHT_POW, mkEdgeMap}
import de.sciss.synth.io.AudioFile
import de.sciss.synth.proc.{Action, AudioCue, AuralSystem, Proc, TimeRef, Transport, WorkspaceHandle}
import de.sciss.synth.{Curve, SynthGraph, proc}

import scala.Predef.{any2stringadd => _, _}
import scala.concurrent.stm.Ref
import scala.util.control.NonFatal

object SoundPaths {
  type S = InMemory

  def main(args: Array[String]): Unit = {
    val edges     = ShowSimilarities.loadAndSortGraph(1::3::Nil, weightPow = WEIGHT_POW, dropAmt = 0.0, mst = false)
    val edgesMST  = MSTKruskal[Vertex, SimEdge](edges)
    // println(s"MST size is ${edgesMST.size}; text 1 has ${BuildSimilarities.readVertices(1).size}; vertices; text 3 has ${BuildSimilarities.readVertices(3).size} vertices")
    val map       = mkEdgeMap(edgesMST)
    val system    = InMemory()
    val aural     = AuralSystem()
    system.step { implicit tx =>
      aural.addClient(new AuralSystem.Client {
        def auralStarted(s: Server)(implicit tx: Txn): Unit = {
          booted(aural, s, edges = edges, edgeMap = map)(system.wrap(tx.peer), system)
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
//    fadeIn .poll(0, "fade-in ")
//    fadeOut.poll(0, "fade-out")
//    val line    = Line.kr(0, 0, dur)
    val env     = Env.linen(attack = fadeIn, sustain = dur - (fadeIn + fadeOut), release = fadeOut, curve = Curve.sine)
    val eg      = EnvGen.ar(env)
    Act(Done.kr(eg), "done")
    val sig     = disk * eg
    Out.ar(0, sig)
  }

  class State(val p: Proc[S], val offset: LongObj.Var[S], val duration: DoubleObj.Var[S],
              val fadeIn: DoubleObj.Var[S], val fadeOut: DoubleObj.Var[S],
              val cueVar: AudioCue.Obj.Var[S], cues: Vec[AudioCue.Obj[S]],
              val t: Transport[S], val vertices1: Vector[Vertex], val vertices2: Vector[Vertex],
              val edges: List[SimEdge], val edgeMap: EdgeMap,
              val rnd: TxnRandom[S]) {

    val alternate = Ref(false)

    val path: Ref[Vector[Vertex]] = Ref(Vector.empty)

    def expandPath(seq0: Vector[Vertex], targetLen: Int)
                  (implicit tx: S#Tx): Vector[Vertex] = {
      val t1        = System.currentTimeMillis()
      var seq1      = seq0
      var sz1       = seq0.size
      println(s"expandPath ${seq0.map(_.quote).mkString(" -- ")} - sz0 is $sz1, targetLen is $targetLen")

      val edgesSet  = edges.toSet
      val edgeMapI  = mkEdgeMap(edges)

      while (sz1 < targetLen) {
        val idxRem  = rnd.nextInt(sz1 - 1)
        val (pre, post) = seq1.splitAt(idxRem + 1)
        val v1i     = pre .last // seq1(idxRem    )
        val v2i     = post.head // seq1(idxRem + 1)
        val edgeRem = if (Vertex.Ord.lt(v1i, v2i))
          Edge(v1i, v2i)(0.0)   // weight doesn't matter for equality
        else
          Edge(v2i, v1i)(0.0)

        println(s"Attempt insert at $idxRem (${edgeRem.start} -- ${edgeRem.end})")
        assert (edgesSet.contains(edgeRem))
        var edgesSetTemp = edgesSet - edgeRem

        edgesSetTemp = (edgesSetTemp /: (pre.init ++ post.tail))((res, v) => res -- edgeMapI.getOrElse(v, Set.empty))

        val edges1    = edgesSetTemp.toList
        val mst1      = MSTKruskal[Vertex, SimEdge](edges1)
        println(s"Remaining edges is ${edges1.size}; mst is ${mst1.size}")
        val edgeMap1  = mkEdgeMap(mst1)
        val seq0i     = ExplorePaths.calcPath(v1i, v2i, edgeMap1).toVector

        if (seq0i.isEmpty) {
          println("WARNING: didn't find a path") // XXX TODO

        } else {
          // edgesSet  = edgesSetTemp
          assert  (seq0i.head == v1i)
          assert  (seq0i.last == v2i)
          val slice     = seq0i.slice(1, seq0i.size - 1)
          seq1          = pre ++ slice ++ post
          sz1           = seq1.size
          println(s"Splicing ${slice.map(_.quote).mkString(" -- ")} -> size is $sz1")
        }
      }

      val t2        = System.currentTimeMillis()
      println(s"... took ${t2-t1}ms")
      seq1
    }

    def newPath(targetLen: Int)(implicit tx: S#Tx): Vector[Vertex] = {
      require(targetLen >= 2)
      val a   = alternate.transformAndGet(! _)
      val vx1 = if (a) vertices1 else vertices2
      val vx2 = if (a) vertices2 else vertices1

      val startIdx  = rnd.nextInt(vx1.size)
      val stopIdx   = rnd.nextInt(vx2.size)

      val v1        = vx1(startIdx)
      val v2        = vx2(stopIdx )

      val seq0      = ExplorePaths.calcPath(v1, v2, edgeMap).toVector
      val sz0       = seq0.size
      val seq1      = if (sz0 >= targetLen) seq0 else {
        expandPath(seq0, targetLen = targetLen)
      }
      val sz1       = seq1.size

      val seq       = if (sz1 == targetLen) {
        seq1
      } else {
        var seq2    = seq1
        var sz2     = sz1
        while (sz2 > targetLen) {
          val idx = rnd.nextInt(sz2 - 1) + 1
          seq2    = seq2.patch(idx, Nil, 1)
          sz2    -= 1
        }
        seq2
      }

      path()        = seq
      println(seq.size)
      seq
    }

    val pathPos = Ref(0)

    def iterate()(implicit tx: S#Tx): Unit = {
      t.stop()
      t.seek(0L)

      val path0: Seq[Vertex] = path()
      val pathPos0: Int = pathPos()
      val (path1, pathPos1) = if (pathPos0 < path0.size) (path0, pathPos0) else {
        val p = try {
          newPath(50)
        } catch {
          case NonFatal(ex) =>
            println("newPath:")
            ex.printStackTrace()
            throw ex
        }
        (p, 0)
      }
      pathPos() = pathPos1 + 1

      val v: Vertex = path1(pathPos1)
      val textIdx   = v.textIdx

      val offV    = framesFromFile(v.span.start)
      val durV    = v.span.length / fileSR
      val fdInV   = v.words.head.fadeIn  * 0.5
      val fdOutV  = v.words.last.fadeOut * 0.5
      offset  ()  = offV
      duration()  = durV
      fadeIn  ()  = fdInV
      fadeOut ()  = fdOutV
      cueVar  ()  = cues(textIdx - 1)

      println(f"vertex ${v.quote}, offset $offV, duration $durV%g, fade-in $fdInV%g, fade-out $fdOutV%g")

      t.play()
    }
  }

  def booted(aural: AuralSystem, s: Server, edges: List[SimEdge], edgeMap: EdgeMap)
            (implicit tx: S#Tx, cursor: stm.Cursor[S]): Unit = {
    val vertices = edgeMap.keys.toVector
    val (vx1, vx2) = vertices.partition(_.textIdx == 1)

    val offVar      = LongObj   .newVar[S](0L)
    val durVar      = DoubleObj .newVar[S](2.0)
    val fadeInVar   = DoubleObj .newVar[S](0.0)
    val fadeOutVar  = DoubleObj .newVar[S](0.0)

    def mkCue(textIdx: Int) = {
      val fileIn      = BuildSimilarities.audioFileIn(textIdx)
      val specIn      = AudioFile.readSpec(fileIn)
      val loc         = ArtifactLocation.newConst[S](fileIn.parent)
      val artIn       = Artifact(loc, fileIn)
      AudioCue.Obj[S](artIn, specIn, offVar, 1.0)
    }

    val cue1 = mkCue(1)
    val cue3 = mkCue(3)

    val p = Proc[S]
    import WorkspaceHandle.Implicits.dummy

    val cue   = AudioCue.Obj.newVar(cue1)
    val rnd   = TxnRandom[S] // (tx.newID())
    val t     = Transport[S](aural)
    val state = new State(p = p, offset = offVar, duration = durVar, fadeIn = fadeInVar, fadeOut = fadeOutVar,
      cueVar = cue, cues = Vector(cue1, null, cue3),
      t = t, vertices1 = vx1, vertices2 = vx2, edges = edges, edgeMap = edgeMap, rnd = rnd)

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