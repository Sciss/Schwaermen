package de.sciss.schwaermen

import scala.util.Random

object TestSpk {
  def main(args: Array[String]): Unit = {
    val network = Spk.default
    implicit val rnd: Random = new Random() // (0L)
    val f = new SpeakerPathFinder(network, maxPathLen = 60)
    val v1 = network.speakers.indexWhere(_.id == 234).toShort // any
    val v2 = network.speakers.indexWhere(_.id == 201).toShort // any
    for (i <- 0 until 40) {
      val path = f.perform(v1, v2)
      println(path.map(_.id).mkString(", "))
    }
  }
}
