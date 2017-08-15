package de.sciss.schwaermen

import scala.util.control.NonFatal

trait MainLike {
  protected def pkgLast: String

  private def buildInfString(key: String): String = try {
    val clazz = Class.forName(s"de.sciss.schwaermen.$pkgLast.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  final def name        : String = "Schwaermen"
  final def namePkg     : String = s"$name ${pkgLast.capitalize}"

  final def version     : String = buildInfString("version")
  final def builtAt     : String = buildInfString("builtAtString")
  final def fullVersion : String = s"$pkgLast v$version, built $builtAt"

  protected def checkConfig(config: ConfigLike): Unit = {
    if (config.disableEnergySaving && !config.isLaptop) {
      import sys.process._
      Seq("xset", "s", "off").!
      Seq("xset", "-dpms").!
    }
  }

  var useLog = false

  def log(what: => String): Unit = if (useLog) {
    println(s"$pkgLast - $what")
  }
}
