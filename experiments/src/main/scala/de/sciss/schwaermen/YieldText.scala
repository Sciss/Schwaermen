/*
 *  YieldText.scala
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

object YieldText {
  final case class Config(idx: Int)

  def main(args: Array[String]): Unit = {
    val default = Config(-1)
    val p = new scopt.OptionParser[Config]("YieldText") {
      opt[Int] ('i', "index")
        .required()
        .action { (v, c) => c.copy(idx = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      import config._
      val words = BuildSimilarities.readVertices(idx).flatMap(_.words).distinct
      words.foreach(println)
      println(words.size)
      println(words.map(_.text).mkString(" "))
    }
  }
}
