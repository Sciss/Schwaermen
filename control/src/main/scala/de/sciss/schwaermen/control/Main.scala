/*
 *  Main.scala
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

package de.sciss.schwaermen.control

import de.sciss.schwaermen.Network

object Main {
  def main(args: Array[String]): Unit = {
    println("-- Schwärmen Control --")
    val default = Config()
    val p = new scopt.OptionParser[Config]("Imperfect-RaspiPlayer") {
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val host = Network.thisIP()
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    val c = OSCClient(config, host)
    new MainFrame(c)
  }

  val packageName = "schwaermen-rpi-sound"
}