/*
 *  CatalogCover.scala
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

import java.awt.geom.Line2D
import java.awt.image.BufferedImage
import java.awt.{BasicStroke, Color, RenderingHints}
import java.io.{DataInputStream, DataOutputStream, FileInputStream, FileOutputStream}

import de.sciss.catmullrom.Point2D
import de.sciss.file._
import de.sciss.kollflitz.Vec
import de.sciss.neuralgas
import de.sciss.neuralgas.ComputeGNG
import de.sciss.schwaermen.CatalogPaths.GNG_COOKIE
import javax.imageio.ImageIO

object CatalogCover {
  case class Config(imgInF        : File    = file("in.png"),
                    imgOutF       : File    = file("out.png"),
                    invert        : Boolean = false,
                    strokeWidth   : Double  = 2.0,
                    rngSeed       : Int     = 0xBEE,
                    maxNodesDecim : Int     = 108,
                    maxNodes      : Int     = 0,
                    gngStepSize   : Int     = 27,
                    gngLambda     : Int     = 27,
                    gngEdgeAge    : Int     = 108,
                    gngEpsilon    : Double  = 0.05,
                    gngEpsilon2   : Double  = 1.0e-4,
                    gngAlpha      : Double  = 0.2,
                    gngBeta       : Double  = 5.0e-6,
                    gngUtility    : Double  = 18.0,
                    interim       : Int     = 2187,
                    interimImages : Boolean = false,
                    widthOut      : Int     = 0,
                    heightOut     : Int     = 0
                   ) {

    lazy val fGNG: File = imgOutF.replaceExt("gng")

    def mkGNGIteration(i: Int): File = {
      val name = s"${fGNG.base}-$i.${fGNG.ext}"
      fGNG.replaceName(name)
    }
  }

  val ex1 = Config(
    imgInF        = file("/data/projects/Schwaermen/photos/170826_Naya/_MG_9905_rot_2zu1gray.jpg"),
    imgOutF       = file("/data/temp/catalog_cover.png"),
    invert        = true,
    maxNodesDecim = 108,
    gngStepSize   = 50,
    gngLambda     = 100,
    gngEdgeAge    = 88,
    gngEpsilon    = 0.05,
    gngEpsilon2   = 1.0e-4
  )

  val ex2 = Config(
    imgInF        = file("/data/projects/Schwaermen/photos/Maryam/_DSC6957rot_crop_gray.jpg"),
    imgOutF       = file("/data/temp/catalog_coverM.png"),
    invert        = true
  )

  val ex3 = Config(
    imgInF      = file("/data/projects/Schwaermen/photos/Maryam/_DSC6866_crop_gray.jpg"),
    imgOutF     = file("/data/temp/catalog_cover6866.png"),
    invert        = true,
    gngEpsilon  = 0.05,
    gngEpsilon2 = 0.05,
    gngAlpha    = 0.1,
    gngBeta     = 1.0e-5,
    interim     = 729,
    interimImages = true
  )

  val selected = Config(
    imgInF      = file("/data/projects/Schwaermen/catalog/_DSC6866_crop_rot_gray.png"),
    imgOutF     = file("/data/projects/Schwaermen/catalog/_DSC6866_crop_rot_gray_GNG.png"),
    invert        = true,
    gngEpsilon  = 0.05,
    gngEpsilon2 = 0.05,
    gngAlpha    = 0.1,
    gngBeta     = 1.0e-5,
    interim     = 0,
    interimImages = false,
    widthOut    = 7282,
    heightOut   = 3650,
    strokeWidth = 2.0   // N.B. this is already scaled by using widthOut and heightOut! 3.4
  )

  val examples: Vector[Config] = Vector(ex1, ex2, ex3)

  def main(args: Array[String]): Unit = {
    val default = Config()
    val p = new scopt.OptionParser[Config]("Schwaermen-Catalog-Cover") {
      opt[File]('i', "input")
//        .required()
        .text (s"Image input file ${default.imgInF})")
        .action { (f, c) => c.copy(imgInF = f) }

      opt[File]('o', "output")
//        .required()
        .text (s"Image output file, should end in '.png' or '.jpg' ${default.imgOutF})")
        .action { (f, c) => c.copy(imgOutF = f) }

      opt[Unit] ("invert")
        .text ("Invert gray scale probabilities.")
        .action { (_, c) => c.copy(invert = true) }

      opt[Double] ("stroke")
        .text (s"Stroke width in pixels (default ${default.strokeWidth})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(strokeWidth = v) }

      opt[Int] ("seed")
        .text (s"Random number generator seed (default ${default.rngSeed})")
        .action { (v, c) => c.copy(rngSeed = v) }

      opt[Int] ('n', "max-nodes")
        .text ("Maximum number of nodes (zero for threshold based)")
        .validate(i => if (i >= 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(maxNodes = v) }

      opt[Int] ("decim")
        .text (s"Pixel decimation to determine maximum number of nodes (default ${default.maxNodesDecim})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(maxNodesDecim = v) }

      opt[Int] ("step")
        .text (s"GNG step size (default ${default.gngStepSize})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngStepSize = v) }

      opt[Int] ("lambda")
        .text (s"GNG lambda parameter (default ${default.gngLambda})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngLambda = v) }

      opt[Int] ("edge-age")
        .text (s"GNG maximum edge age (default ${default.gngEdgeAge})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEdgeAge = v) }

      opt[Double] ("eps")
        .text (s"GNG epsilon parameter (default ${default.gngEpsilon})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEpsilon = v) }

      opt[Double] ("eps2")
        .text (s"GNG epsilon 2 parameter (default ${default.gngEpsilon2})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngEpsilon2 = v) }

      opt[Double] ("alpha")
        .text (s"GNG alpha parameter (default ${default.gngAlpha})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngAlpha = v) }

      opt[Double] ("beta")
        .text (s"GNG beta parameter (default ${default.gngBeta})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngBeta = v) }

      opt[Double] ("utility")
        .text (s"GNG-U utility parameter (default ${default.gngUtility})")
        .validate(i => if (i > 0) Right(()) else Left("Must be > 0") )
        .action { (v, c) => c.copy(gngUtility = v) }

      opt[Int] ('t', "interim")
        .text (s"Interim file output, every n steps, or zero to turn off (default ${default.interim})")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(interim = v) }

      opt[Unit] ("interim-images")
        .text ("Generate images for interim files.")
        .action { (_, c) => c.copy(interimImages = true) }

      opt[Int] ("ex")
        .text (s"Use example <n> parameters (1 to ${examples.size}")
        .validate(i => if (i >= 1 && i <= examples.size) Right(()) else Left(s"Must be >= 1 and <= ${examples.size}") )
        .action { (i, _) => examples(i - 1) }

      opt[Unit] ("selected")
        .text ("Use the parameters selected for the catalog cover.")
        .action { (_, _) => selected }

      opt[Int] ('w', "width")
        .text (s"Rendering output width in pixels, or zero to match input.")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(widthOut = v) }

      opt[Int] ('h', "height")
        .text (s"Rendering output height in pixels, or zero to match input.")
        .validate(i => if (i >= 0) Right(()) else Left("Must be >= 0") )
        .action { (v, c) => c.copy(heightOut = v) }
    }
    p.parse(args, default).fold(sys.exit(1)) { config =>
      require(config.imgInF.isFile, s"Input image ${config.imgInF} does not exist.")
      run(config)
    }
  }

  def run(config: Config): Unit = {
    if (config.fGNG.length() > 0L) {
      println(s"GNG file ${config.fGNG} already exists. Not overwriting.")
    } else {
      runGNG(config)
    }

    if (config.imgOutF.length() > 0L) {
      println(s"Image file ${config.imgOutF} already exists. Not overwriting.")
    } else {
      renderImage(config)()
    }
  }

  def renderImage(config: Config, quiet: Boolean = false)
                 (fGNG: File = config.fGNG, fImgOut: File = config.imgOutF): Unit = {
    val graph = readGNG(fGNG)
    val wIn   = graph.surfaceWidthPx
    val hIn   = graph.surfaceHeightPx
    val wOut  = if (config.widthOut  == 0) wIn else config.widthOut
    val hOut  = if (config.heightOut == 0) hIn else config.heightOut
    val sx    = wOut.toDouble / wIn
    val sy    = hOut.toDouble / hIn
    val img   = new BufferedImage(wOut, hOut, BufferedImage.TYPE_BYTE_GRAY)
    val g     = img.createGraphics()
    g.setColor(Color.white)
    g.fillRect(0, 0, wOut, hOut)
    if (sx != 1.0 || sy != 1.0) g.scale(sx, sy)
    g.setColor(Color.black)
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON   )
    g.setRenderingHint(RenderingHints.KEY_RENDERING     , RenderingHints.VALUE_RENDER_QUALITY )
    g.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE    )
    g.setStroke(new BasicStroke(config.strokeWidth.toFloat))
    val ln = new Line2D.Double()
    graph.edges.foreach { case CatalogPaths.Edge(from, to) =>
      val nFrom = graph.nodes(from)
      val nTo   = graph.nodes(to  )
      ln.setLine(nFrom.x, nFrom.y, nTo.x, nTo.y)
      g.draw(ln)
    }
    g.dispose()
    val fmt   = if (fImgOut.extL == "png") "png" else "jpg"
    ImageIO.write(img, fmt, fImgOut)
    if (!quiet) println(s"Wrote ${fImgOut.name}")
  }

  case class ResGNG(surfaceWidthPx: Int, surfaceHeightPx: Int, nodes: Vec[Point2D],
                    edges: Vec[CatalogPaths.Edge])

  def readGNG(fGNG: File): ResGNG = {
    val fis = new FileInputStream(fGNG)
    try {
      val dis = new DataInputStream(fis)
      require (dis.readInt() == GNG_COOKIE)
      val surfaceWidthPx  = dis.readShort()
      val surfaceHeightPx = dis.readShort()
      val nNodes = dis.readInt()
      val nodes = Vector.fill(nNodes) {
        val x = dis.readFloat()
        val y = dis.readFloat()
        Point2D(x, y)
      }
      val nEdges = dis.readInt()
      val edges = Vector.fill(nEdges) {
        val from = dis.readInt()
        val to   = dis.readInt()
        CatalogPaths.Edge(from, to)
      }
      ResGNG(surfaceWidthPx, surfaceHeightPx, nodes, edges)

    } finally {
      fis.close()
    }
  }

  def runGNG(config: Config): Unit = {
    import config._
    val img       = ImageIO.read(imgInF)
    val c         = new ComputeGNG
    val pd        = new GrayImagePD(img, invert = invert)
    c.pd          = pd
    val w         = img.getWidth
    val h         = img.getHeight
    c.panelWidth  = w // / 8
    c.panelHeight = h // / 8
    c.maxNodes    = if (maxNodes > 0) maxNodes else pd.getNumPixels / maxNodesDecim
    println(s"w ${c.panelWidth}, h ${c.panelHeight}, maxNodes ${c.maxNodes}")
    c.stepSize    = gngStepSize
    c.algorithm   = neuralgas.Algorithm.GNGU
    c.lambdaGNG   = gngLambda
    c.maxEdgeAge  = gngEdgeAge
    c.epsilonGNG  = gngEpsilon  .toFloat
    c.epsilonGNG2 = gngEpsilon2 .toFloat
    c.alphaGNG    = gngAlpha    .toFloat
    c.setBetaGNG(gngBeta.toFloat)
    c.noNewNodesGNGB = false
    c.GNG_U_B     = true
    c.utilityGNG  = gngUtility  .toFloat
    c.autoStopB   = false
    c.reset()
    //    c.getRNG.setSeed(108L)
    c.getRNG.setSeed(rngSeed)
    c.addNode(null)
    c.addNode(null)

    val interimF      = if (interim != 0) interim else 729
    val res           = new ComputeGNG.Result
    var lastProgress  = 0
    var iteration     = 0
    var lastN         = -1
    var lastE         = -1
    val t0            = System.currentTimeMillis()
    println("_" * 100)
    while (!res.stop && c.nNodes < c.maxNodes) {
      c.learn(res)
      val progress: Int = (c.nNodes * 100) / c.maxNodes
      if (lastProgress < progress) {
        while (lastProgress < progress) {
          print('#')
          lastProgress += 1
        }
      }
      iteration += 1
      if (iteration % interimF == 0) {
        if (interim > 0) {
          val fTemp = mkGNGIteration(iteration)
          writeGNG(c, fTemp, w = w, h = h)
          if (interimImages) {
            val fTempImg = fTemp.replaceExt(imgOutF.ext)
            renderImage(config, quiet = true)(fGNG = fTemp, fImgOut = fTempImg)
          }
        }
        if (c.nNodes <= lastN && c.nEdges <= lastE) {
          res.stop = true
        } else {
          lastN = c.nNodes
          lastE = c.nEdges
        }
      }
    }

    println(s" Done GNG. ${c.numSignals} signals, took ${(System.currentTimeMillis() - t0) / 1000} sec.")
    writeGNG(c, fGNG, w = w, h = h)
  }

  def writeGNG(c: ComputeGNG, fOut: File, w: Int, h: Int): Unit = {
    val fos = new FileOutputStream(fOut)
    try {
      val dos = new DataOutputStream(fos)
      dos.writeInt(GNG_COOKIE)
      dos.writeShort(w)
      dos.writeShort(h)
      dos.writeInt(c.nNodes)
      for (i <- 0 until c.nNodes) {
        val n = c.nodes(i)
        dos.writeFloat(n.x)
        dos.writeFloat(n.y)
      }
      dos.writeInt(c.nEdges)
      for (i <- 0 until c.nEdges) {
        val e = c.edges(i)
        dos.writeInt(e.from)
        dos.writeInt(e.to  )
      }

    } finally {
      fos.close()
    }
  }
}
