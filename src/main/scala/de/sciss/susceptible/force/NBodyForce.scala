/*
 *  NBodyForce.scala
 *  (Susceptible)
 *
 *  Copyright (c) 2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.susceptible.force

import prefuse.util.force.{AbstractForce, ForceItem, ForceSimulator}

import scala.collection.mutable
import scala.util.Random

/** Basically a Scala translation of the original Prefuse code,
  * plus adjustable RNG seed.
  */
object NBodyForce {
  private val pNames: Array[String] = Array[String]("GravitationalConstant", "Distance", "BarnesHutTheta")

  final val GRAVITATIONAL_CONST       : Int   = 0
  final val MIN_DISTANCE              : Int   = 1
  final val BARNES_HUT_THETA          : Int   = 2

  final val DEFAULT_GRAV_CONSTANT     : Float = -1.0f
  final val DEFAULT_MIN_GRAV_CONSTANT : Float = -10f
  final val DEFAULT_MAX_GRAV_CONSTANT : Float = +10f

  final val DEFAULT_DISTANCE          : Float = -1f
  final val DEFAULT_MIN_DISTANCE      : Float = -1f
  final val DEFAULT_MAX_DISTANCE      : Float = 500f

  final val DEFAULT_THETA             : Float = 0.9f
  final val DEFAULT_MIN_THETA         : Float = 0.0f
  final val DEFAULT_MAX_THETA         : Float = 1.0f
}
class NBodyForce(gravConstant : Float = NBodyForce.DEFAULT_GRAV_CONSTANT,
                 minDistance  : Float = NBodyForce.DEFAULT_DISTANCE,
                 theta        : Float = NBodyForce.DEFAULT_THETA)
  extends AbstractForce {

  import NBodyForce._

  private val rand  = new Random(12345678L)
  private var root  = factory.getQuadTreeNode
  private var xMin  = 0f
  private var xMax  = 0f
  private var yMin  = 0f
  private var yMax  = 0f

  params    = Array[Float](gravConstant, minDistance, theta)
  minValues = Array[Float](DEFAULT_MIN_GRAV_CONSTANT, DEFAULT_MIN_DISTANCE, DEFAULT_MIN_THETA)
  maxValues = Array[Float](DEFAULT_MAX_GRAV_CONSTANT, DEFAULT_MAX_DISTANCE, DEFAULT_MAX_THETA)

  def setSeed(n: Long): Unit = rand.setSeed(n)

  /**
    * Returns true.
    * @see prefuse.util.force.Force#isItemForce()
    */
  override def isItemForce: Boolean = true

  /**
    * @see prefuse.util.force.AbstractForce#getParameterNames()
    */
  protected def getParameterNames: Array[String] = pNames

  /**
    * Set the bounds of the region for which to compute the n-body simulation
    * @param xMin the minimum x-coordinate
    * @param yMin the minimum y-coordinate
    * @param xMax the maximum x-coordinate
    * @param yMax the maximum y-coordinate
    */
  private def setBounds(xMin: Float, yMin: Float, xMax: Float, yMax: Float): Unit = {
    this.xMin = xMin
    this.yMin = yMin
    this.xMax = xMax
    this.yMax = yMax
  }

  /**
    * Clears the quadtree of all entries.
    */
  def clear(): Unit = {
    clearHelper(root)
    root = factory.getQuadTreeNode
  }

  private def clearHelper(n: QuadTreeNode): Unit = {
    var i = 0
    while (i < n.children.length) {
      if (n.children(i) != null) clearHelper(n.children(i))
      i += 1
    }
    factory.reclaim(n)
  }

  /**
    * Initialize the simulation with the provided enclosing simulation. After
    * this call has been made, the simulation can be queried for the
    * n-body force acting on a given item.
    * @param fsim the enclosing ForceSimulator
    */
  override def init(fsim: ForceSimulator): Unit = {
    clear()
    var x1  = Float.MaxValue
    var y1  = Float.MaxValue
    var x2  = Float.MinValue
    var y2  = Float.MinValue

    val it1: java.util.Iterator[_] = fsim.getItems
    while (it1.hasNext) {
      val item  = it1.next.asInstanceOf[ForceItem]
      val x     = item.location(0)
      val y     = item.location(1)
      if (x < x1) x1 = x
      if (y < y1) y1 = y
      if (x > x2) x2 = x
      if (y > y2) y2 = y
    }

    val dx  = x2 - x1
    val dy  = y2 - y1

    if (dx > dy)
      y2 = y1 + dx
    else
      x2 = x1 + dy

    setBounds(x1, y1, x2, y2)

    val it2: java.util.Iterator[_] = fsim.getItems
    while (it2.hasNext) {
      val item = it2.next.asInstanceOf[ForceItem]
      insert(item)
    }

    calcMass(root)
  }

  /**
    * Inserts an item into the quadtree.
    * @param item the ForceItem to add.
    * @throws IllegalStateException if the current location of the item is
    *                               outside the bounds of the quadtree
    */
  def insert(item: ForceItem): Unit =
    try {
      insert(item, root, xMin, yMin, xMax, yMax)
    }
    catch {
      case e: StackOverflowError => e.printStackTrace()
    }

  private def insert(p: ForceItem, n: QuadTreeNode, x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
    if (n.hasChildren) {
      insertHelper(p, n, x1, y1, x2, y2)
    }
    else if (n.value != null) {
      if (isSameLocation(n.value, p)) {
        insertHelper(p, n, x1, y1, x2, y2)
      }
      else {
        val v: ForceItem = n.value
        n.value = null
        insertHelper(v, n, x1, y1, x2, y2)
        insertHelper(p, n, x1, y1, x2, y2)
      }
    }
    else {
      n.value = p
    }
  }

  private def isSameLocation(f1: ForceItem, f2: ForceItem): Boolean = {
    val dx = math.abs(f1.location(0) - f2.location(0))
    val dy = math.abs(f1.location(1) - f2.location(1))
    dx < 0.01f && dy < 0.01f
  }

  private def insertHelper(p: ForceItem, n: QuadTreeNode, x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
    val x       = p.location(0)
    val y       = p.location(1)
    val splitX  = (x1 + x2) / 2
    val splitY  = (y1 + y2) / 2

    val i = (if (x >= splitX) 1 else 0) + (if (y >= splitY) 2 else 0)
    if (n.children(i) == null) {
      n.children(i) = factory.getQuadTreeNode
      n.hasChildren = true
    }
    val x1b = if (i == 1 || i == 3) splitX  else x1
    val x2b = if (i == 1 || i == 3) x2      else splitX
    val y1b = if (i > 1)            splitY  else y1
    val y2b = if (i > 1)            y2      else splitY
    insert(p, n.children(i), x1b, y1b, x2b, y2b)
  }

  private def calcMass(n: QuadTreeNode): Unit = {
    var xCom = 0f
    var yCom = 0f
    n.mass = 0
    if (n.hasChildren) {
      var i = 0
      while (i < n.children.length) {
        val c = n.children(i)
        if (c != null) {
          calcMass(c)
          val m   = c.mass
          n.mass += m
          xCom   += m * c.com(0)
          yCom   += m * c.com(1)
        }
        i += 1
      }
    }
    if (n.value != null) {
      val c   = n.value
      val m   = c.mass
      n.mass += m
      xCom   += m * c.location(0)
      yCom   += m * c.location(1)
    }
    n.com(0) = xCom / n.mass
    n.com(1) = yCom / n.mass
  }

  override def getForce(item: ForceItem): Unit =
    try {
      forceHelper(item, root, xMin, yMin, xMax, yMax)
    }
    catch {
      case e: StackOverflowError => e.printStackTrace()
    }

  /*
   * Helper minimize number of object creations across multiple
   * uses of the quadtree.
   */
  private object factory {
    private val maxNodes: Int = 50000
    private val nodes = mutable.Buffer.empty[QuadTreeNode]

    def getQuadTreeNode: QuadTreeNode =
      if (nodes.nonEmpty)
        nodes.remove(nodes.size - 1)
      else
        new QuadTreeNode

    def reclaim(n: QuadTreeNode): Unit = {
      n.mass        = 0
      n.com(0)      = 0.0f
      n.com(1)      = 0.0f
      n.value       = null
      n.hasChildren = false
      val c         = n.children
      c(0) = null
      c(1) = null
      c(2) = null
      c(3) = null
      if (nodes.size < maxNodes) nodes += n
    }
  }

  private final class QuadTreeNode {
    var hasChildren : Boolean             = false
    var mass        : Float               = 0f
    var com         : Array[Float]        = new Array[Float](2)
    var value       : ForceItem           = _
    var children    : Array[QuadTreeNode] = new Array[QuadTreeNode](4)
  }

  private def forceHelper(item: ForceItem, n: QuadTreeNode, x1: Float, y1: Float, x2: Float, y2: Float): Unit = {
    var dx  = n.com(0) - item.location(0)
    var dy  = n.com(1) - item.location(1)

    val same = dx == 0f && dy == 0f

    if (same) {
      dx    = (rand.nextFloat() - 0.5f) / 50.0f
      dy    = (rand.nextFloat() - 0.5f) / 50.0f
    }

    val r         = math.sqrt(dx * dx + dy * dy).toFloat
    val isMinDist = params(MIN_DISTANCE) > 0f && r > params(MIN_DISTANCE)

    if ((!n.hasChildren && n.value != item) || (!same && (x2 - x1) / r < params(BARNES_HUT_THETA))) {
      if (isMinDist) return

      val v = params(GRAVITATIONAL_CONST) * item.mass * n.mass / (r * r * r)
      item.force(0) += v * dx
      item.force(1) += v * dy
    }
    else if (n.hasChildren) {
      val splitX  = (x1 + x2) / 2
      val splitY  = (y1 + y2) / 2

      var i = 0
      while (i < n.children.length) {
        if (n.children(i) != null) {
          forceHelper(item, n.children(i),
            x1 = if (i == 1 || i == 3) splitX else x1,
            y1 = if (i > 1) splitY else y1,
            x2 = if (i == 1 || i == 3) x2 else splitX,
            y2 = if (i > 1) y2 else splitY)
        }
        i += 1
      }

      if (isMinDist) return

      if (n.value != null && n.value != item) {
        val v = params(GRAVITATIONAL_CONST) * item.mass * n.value.mass / (r * r * r)
        item.force(0) += v * dx
        item.force(1) += v * dy
      }
    }
  }
}