package de.sciss.schwaermen

object YieldText {
  def main(args: Array[String]): Unit = {
    val words = BuildSimilarities.readVertices(3).flatMap(_.words).distinct
    words.foreach(println)
    println(words.size)
    println(words.map(_.text).mkString(" "))
  }
}
