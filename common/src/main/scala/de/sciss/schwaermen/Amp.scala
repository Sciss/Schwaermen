package de.sciss.schwaermen

import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.file._

import scala.util.control.NonFatal

object Amp {
  final val numChannels = 18
}
final class Amp(baseDir: File) {
  import Amp.numChannels

  private[this] val f = baseDir / "amps.bin"

  def load(): Array[Float] =
    try {
      val fin = new FileInputStream(f)
      try {
        val din = new DataInputStream(fin)
        Array.fill[Float](18)(din.readFloat())
      } finally {
        fin.close()
      }

    } catch {
      case NonFatal(_) =>
        Array.fill(numChannels)(1f)
    }

  def save(): Unit =
    try {
      val fout = new FileOutputStream(f)
      try {
        val dout = new DataOutputStream(fout)
        volumes.foreach(dout.writeFloat)
      } finally {
        fout.close()
      }

    } catch {
      case NonFatal(_) =>
        Array.fill(numChannels)(1f)
    }

  val volumes: Array[Float] = load()
}
