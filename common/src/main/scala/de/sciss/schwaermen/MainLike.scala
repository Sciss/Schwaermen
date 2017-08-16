package de.sciss.schwaermen

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import scala.annotation.elidable
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

  var showLog = false

  private[this] lazy val logHeader = new SimpleDateFormat(s"[HH:mm''ss.SSS] '$pkgLast' - ", Locale.US)

  @elidable(elidable.CONFIG) def log(what: => String): Unit =
    if (showLog) println(logHeader.format(new Date()) + what)
}
