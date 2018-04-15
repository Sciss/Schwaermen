package de.sciss.schwaermen

import java.awt.Dimension
import java.awt.Graphics
import java.awt.image.BufferedImage
import java.util

import de.sciss.neuralgas.{ComputeGNG, PD, PanelLike}

class GrayImagePD(val img: BufferedImage, val invert: Boolean) extends PD {
  private[this] val w         : Int = img.getWidth
  private[this] val h         : Int = img.getHeight
  private[this] val numPixels : Int = w * h

  private[this] val dots: Array[Double] = {
    val _dots = new Array[Double](numPixels)
    var sum = 0.0
    var i = 0
    var x = 0
    while (x < w) {
      var y = 0
      while (y < h) {
        val rgb     = img.getRGB(x, y)
        val red     = ((rgb & 0xFF0000) >> 16) / 255.0
        val green   = ((rgb & 0x00FF00) >>  8) / 255.0
        val blue    = ((rgb & 0x00FF00) >>  8) / 255.0
        val lum     = 0.2126 * red + 0.7152 * green + 0.0722 * blue
        val value   = if (invert) 1.0 - lum else lum
        sum += value
        _dots(i) = sum
        i += 1
        y += 1
      }
      x += 1
    }
    i = 0
    val mul = 1.0 / sum
    while (i < numPixels) {
      _dots(i) *= mul
      i += 1
    }

    _dots
  }

  def getNumPixels: Int = numPixels

  override def getSignal(compute: ComputeGNG): Unit = {
    val wi    = compute.panelWidth
    val hi    = compute.panelHeight
    val _dots = dots
    val i0    = util.Arrays.binarySearch(_dots, compute.random)
    val i1    = if (i0 >= 0) i0 else -(i0 - 1)
    val dot    = if (i1 < _dots.length) i1 else _dots.length - 1
    val _h    = h
    val xIn   = dot / _h
    val yIn   = dot % _h
    compute.SignalX = (xIn * wi).toFloat /  w
    compute.SignalY = (yIn * hi).toFloat / _h
  }

  override def draw(compute: ComputeGNG, panel: PanelLike, g: Graphics, d: Dimension): Unit = {
    val wi = d.width
    val hi = d.height
    g.drawImage(img, 0, 0, wi, hi, null)
  }

  override def getName = "GImage"

  override def ordinal: Int = -1
}