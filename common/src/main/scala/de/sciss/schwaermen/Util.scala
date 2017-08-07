/*
 *  Util.scala
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

object Util {
  def shutdown(): Unit = {
    import sys.process._
    Seq("sudo", "shutdown", "now").run()
  }

  def reboot(): Unit = {
    import sys.process._
    Seq("sudo", "reboot", "now").run()
  }

  private[this] val uniqueSync  = new AnyRef
  private[this] var uniqueID    = 0

  def nextUniqueID(): Int = uniqueSync.synchronized {
    val res = uniqueID
    uniqueID += 1
    res
  }

  def readTextResource(name: String): String = {
    val url = getClass.getResource(name)
    require(url != null)
    val is  = url.openStream()
    val sz  = is.available()
    val arr = new Array[Byte](sz)
    is.read(arr)
    is.close()
    new String(arr, "UTF-8")
  }
}
