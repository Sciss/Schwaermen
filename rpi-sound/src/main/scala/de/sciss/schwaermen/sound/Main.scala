package de.sciss.schwaermen.sound

import scala.util.control.NonFatal

object Main {
  private def buildInfString(key: String): String = try {
    val clazz = Class.forName("de.sciss.schwaermen.sound.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  def main(args: Array[String]): Unit = {
    println(s"-- Schwärmen Sound v${buildInfString("version")}, built ${buildInfString("builtAtString")} --")
  }
}
