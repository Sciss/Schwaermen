package de.sciss.schwaermen

import scala.util.control.NonFatal

trait HasBuildInfo {
  protected def buildInfoPackage: String

  private def buildInfString(key: String): String = try {
    val clazz = Class.forName(s"$buildInfoPackage.BuildInfo")
    val m     = clazz.getMethod(key)
    m.invoke(null).toString
  } catch {
    case NonFatal(_) => "?"
  }

  final def version     : String = buildInfString("version")
  final def builtAt     : String = buildInfString("builtAtString")
  final def fullVersion : String = s"v$version, built $builtAt"
}
