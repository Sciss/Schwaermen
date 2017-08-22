/*
 *  BinarySimilarities.scala
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

import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream, IOException}

import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.schwaermen.BuildSimilarities.{SimEdge, Vertex}

import scala.collection.breakOut
import scala.util.Random

object BinarySimilarities {
  private final val COOKIE = 0x45646765 // "Edge"

  final case class Config(idxA: Int, idxB: Int, fOut: File)

  def main(args: Array[String]): Unit = {
    val default = Config(-1, -1, file("out"))
    val p = new scopt.OptionParser[Config]("BuildSimilarities") {
      opt[Int] ('a', "first-index")
        .required()
        .action { (v, c) => c.copy(idxA = v) }

      opt[Int] ('b', "second-index")
        .required()
        .action { (v, c) => c.copy(idxB = v) }

      opt[File] ('f', "output")
        .required()
        .action { (v, c) => c.copy(fOut = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      import config._
      convert(textId1 = idxA, textId2 = idxB, fOut = fOut)
    }
  }

  /** Converts the output from `BuildSimilarities` into a reduced and
    * more regular format easy to read for `PathFinder`.
    * Among other things, produces _all_ edges
    */
  def convert(textId1: Int, textId2: Int, fOut: File): Unit = {
    if (fOut.exists()) {
      Console.err.println(s"File $fOut already exists. Not overwriting!")
      return
    }

    val simEdge: List[SimEdge] =
      ShowSimilarities.loadAndSortGraph(textIndices = textId1 :: textId2 :: Nil, mst = false)

    val simEdgeR = simEdge.reverse
//    val edgeMap = edges.groupBy(_.start.index).map {
//      case (start, targets) => start -> targets.groupBy(_.end)
//    }
    val allVertices: Vec[Vertex] = {
      val tmp: Vec[Vertex] = simEdge.flatMap(e => e.start :: e.end :: Nil)(breakOut)
      tmp.distinct.sorted
    }
    val numVertices     = allVertices.size
    println(s"numVertices = $numVertices")
    val textLen1        = allVertices.indexWhere(_.textIdx == textId2)
    require(textLen1 > 0)
    val textLen2        = numVertices - textLen1
    val vertexIndexMap  = allVertices.zipWithIndex.toMap

    val allEdgesSorted: Array[Int] = simEdgeR.map { e =>
      val v1i   = vertexIndexMap(e.start)
      val v2i   = vertexIndexMap(e.end  )
      val start = if (v1i < v2i) v1i else v2i
      val end   = if (v1i < v2i) v2i else v1i
      (start << 16) | end
    } (breakOut)

    val fos = new FileOutputStream(fOut)
    try {
      val dos = new DataOutputStream(fos)
      dos.writeInt(COOKIE)
      dos.writeByte   (textId1)
      dos.writeShort  (textLen1)
      dos.writeByte   (textId2)
      dos.writeShort  (textLen2)
      dos.writeInt    (allEdgesSorted.length)
      allEdgesSorted.foreach(dos.writeInt)

    } finally {
      fos.close()
    }
  }

  def read(fIn: File, maxPathLen: Int)(implicit rnd: Random): PathFinder = {
    val fis = new FileInputStream(fIn)
    try {
      val dis         = new DataInputStream(fis)
      val cookie      = dis.readInt()
      if (cookie != COOKIE) throw new IOException(s"File $fIn does not have magic cookie ${COOKIE.toHexString}")
      /* val textId1 = */ dis.readByte  ()
      val textLen1    = dis.readShort ()
      /* val textId2 = */ dis.readByte  ()
      val textLen2    = dis.readShort ()
      val numVertices = textLen1 + textLen2
      val numEdges    = dis.readInt   ()
      val edgesSorted = new Array[Int](numEdges)
      var edgeIdx     = 0
      while (edgeIdx < numEdges) {
        edgesSorted(edgeIdx) = dis.readInt()
        edgeIdx += 1
      }
      new PathFinder(numVertices = numVertices, allEdgesSorted = edgesSorted, maxPathLen = maxPathLen)

    } finally {
      fis.close()
    }
  }
}
