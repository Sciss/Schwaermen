package de.sciss.schwaermen

import scala.util.Random

object TestSpk {
  def main(args: Array[String]): Unit = {
    val network = Spk.default
    implicit val rnd: Random = new Random() // (0L)
    val f = new SpeakerPathFinder(network, maxPathLen = 108 /* 30 */)
    val v1 = network.speakers.indexWhere(_.id == 137).toShort
    val v2 = network.speakers.indexWhere(_.id == 238).toShort
    val v3 = network.speakers.indexWhere(_.id == 333).toShort

    Seq(v1 -> v2, v1 -> v3, v2 -> v1, v2 -> v3, v3 -> v1, v3 -> v2).foreach { case (va, vb) =>
      for (_ <- 0 until 40000) {
        val path = f.findPath(va, vb)
        assert (path.nonEmpty)
        print(f"len ${path.length}%03d: ")
        println(path.map(_.id).mkString(", "))
      }
    }
  }
}
