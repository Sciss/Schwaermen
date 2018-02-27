/*
 *  ScaleVideo.scala
 *  (Schwaermen)
 *
 *  Copyright (c) 2017-2018 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.schwaermen

import javax.imageio.ImageIO

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.stream.Control

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future, Promise}

object ScaleVideo {
  case class Config(inputTemp     : File    = file("input-%d.jpg"),
                    outputTemp    : File    = file("output-%d.jpg"),
                    startFrame    : Int     = 1,
                    endFrame      : Int     = 1000,
                    marginLeft    : Double  = 0,
                    marginTop     : Double  = 0,
                    marginBottom  : Double  = 0,
                    marginRight   : Double  = 0,
                    aspect        : Boolean = false,
                    zeroCrossings : Int     = 15,
                    jpgQuality    : Int     = 95
                   )

  def main(args: Array[String]): Unit = {
    val default = Config()

    val p = new scopt.OptionParser[Config]("Moor-Study") {
      opt[File]("input")
        .text ("Input template - use %d as place-holder for frame number")
        .required()
        .action { (f, c) => c.copy(inputTemp = f) }

      opt[File]("output")
        .text ("Output template - use %d as place-holder for frame number")
        .required()
        .action { (f, c) => c.copy(outputTemp = f) }

      opt[Int] ("start-frame")
        .text (s"First frame in input template (inclusive; default: ${default.startFrame})")
        .action   { (v, c) => c.copy(startFrame = v) }

      opt[Int] ("end-frame")
        .text ("Last frame in input template (inclusive)")
        .required()
        .action   { (v, c) => c.copy(endFrame = v) }

      opt[Double] ('l', "left")
        .text (s"Left margin (default: ${default.marginLeft})")
        .action   { (v, c) => c.copy(marginLeft = v) }

      opt[Double] ('t', "top")
        .text (s"Left margin (default: ${default.marginTop})")
        .action   { (v, c) => c.copy(marginTop = v) }

      opt[Double] ('b', "bottom")
        .text (s"Bottom margin (default: ${default.marginBottom})")
        .action   { (v, c) => c.copy(marginBottom = v) }

      opt[Double] ('r', "right")
        .text (s"Right margin (default: ${default.marginRight})")
        .action   { (v, c) => c.copy(marginRight = v) }

      opt[Unit] ('a', "aspect")
        .text ("Preserve aspect ratio")
        .action   { (_, c) => c.copy(aspect = true) }

      opt[Int] ('z', "zero-crossings")
        .text (s"Resampling filter zero crossings, or zero for bicubic interpolation (default: ${default.zeroCrossings})")
        .action   { (v, c) => c.copy(zeroCrossings = v) }

      opt[Int] ("jpg-quality")
        .text (s"JPEG quality (default: ${default.jpgQuality})")
        .action   { (v, c) => c.copy(jpgQuality = v) }
    }
    p.parse(args, default).fold(sys.exit(1))(run)
  }

  def run(config: Config): Unit = {
    val (g, fut) = mkGraph(config)
    val c = Control()
    c.run(g)
    Await.result(fut, Duration.Inf)
    println("Done.")
  }

  private def format(f: File, n: Int): File = {
    val name = f.name.format(n)
    f.parentOption.fold(file(name))(_ / name)
  }

  private def mkGraph(config: Config): (Graph, Future[Unit]) = {
    import config._

    val p = Promise[Double]()

    val f1 = format(inputTemp, startFrame)
    val (width, height) = {
      val i = ImageIO.read(f1)
      val res = (i.getWidth(), i.getHeight())
      i.flush()
      res
    }

    val g = Graph {
      import de.sciss.fscape._
      import graph._

      def indices = ArithmSeq(startFrame, length = endFrame - startFrame + 1)
      val imgIn   = ImageFileSeqIn(inputTemp, numChannels = 3, indices = indices)
      val sx0     = width  / (width  - (marginLeft + marginRight))
      val sy0     = height / (height - (marginTop  + marginBottom))
      val (sx, sy) = if (!aspect) (sx0, sy0) else {
        val m = math.max(sx0, sy0)
        (m, m)
      }

      val tx      = -marginLeft // * sx // ?
      val ty      = -marginTop  // * sy // ?

      println(s"width $width, height $height, sx $sx, sy $sy, tx $tx, ty $ty")

      val scaled = AffineTransform2D(imgIn,
        widthIn = width, heightIn = height, widthOut = width, heightOut = height,
        m00 = sx, m10 = 0.0, m01 = 0.0, m11 = sy, m02 = tx, m12 = ty, wrap = 1,
        zeroCrossings = zeroCrossings)

      val sig = scaled.max(0.0).min(1.0)

      val fmt       = if (outputTemp.extL == "png") ImageFile.Type.PNG else ImageFile.Type.JPG
      val specOut   = ImageFile.Spec(fileType = fmt, width = width, height = height, numChannels = 3,
        quality = jpgQuality)

      ImageFileSeqOut(outputTemp, specOut, indices = indices, in = sig)

      Fulfill(sig.last, p)
    }

    import scala.concurrent.ExecutionContext.Implicits.global
    (g, p.future.map(_ => ()))
  }
}
