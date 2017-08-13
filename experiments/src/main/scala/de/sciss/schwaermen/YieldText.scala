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
  def main(args: Array[String]): Unit = {
    val words = BuildSimilarities.readVertices(1).flatMap(_.words).distinct
    words.foreach(println)
    println(words.size)
    println(words.map(_.text).mkString(" "))
  }
}
