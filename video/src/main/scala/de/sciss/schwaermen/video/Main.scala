package de.sciss.schwaermen
package video


import de.sciss.file.File

object Main extends HasBuildInfo {
  protected val buildInfoPackage = "de.sciss.schwaermen.video"

  final val name = "Schwaermen Video"

  def main(args: Array[String]): Unit = {
    println(s"-- $name $fullVersion --")
    val default = Config()
    val p = new scopt.OptionParser[Config](name) {
      opt[File]("base-dir")
        .text (s"Base directory (default: ${default.baseDir})")
        // .required()
        .action { (f, c) => c.copy(baseDir = f) }

      opt[Unit] ('d', "dump-osc")
        .text (s"Enable OSC dump (default ${default.dumpOSC})")
        .action { (_, c) => c.copy(dumpOSC = true) }

      opt[Unit] ("laptop")
        .text (s"Instance is laptop (default ${default.isLaptop})")
        .action { (_, c) => c.copy(isLaptop = true) }

      //      opt[Unit] ("test-pins")
      //        .action { (_, c) => TEST_PINS = true; c }
      //
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      val host = Network.thisIP()
      if (!config.isLaptop) {
        Network.compareIP(host)
//        // cf. https://github.com/Pi4J/pi4j/issues/238
//        try {
//          GpioUtil.enableNonPrivilegedAccess()
//        } catch {
//          case NonFatal(ex) =>
//            Console.err.println("Could not enable GPIO access")
//            ex.printStackTrace()
//        }
      }
      run(host, config)
    }
  }

  def run(host: String, config: Config): Unit = {
    val c = OSCClient(config, host)
    new Heartbeat(c)
  }
}