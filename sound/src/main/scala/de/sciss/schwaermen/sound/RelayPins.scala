/*
 *  RelayPins.scala
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

package de.sciss.schwaermen.sound

import com.pi4j.io.gpio.{GpioFactory, GpioPinDigitalOutput, Pin, RaspiPin}

import scala.collection.immutable.{IndexedSeq => Vec}

object RelayPins {
  final val default: RelayPins = new RelayPins(
    left  = Vector( 2,  3, 14, 15, 17, 18),
    right = Vector(22, 23, 10,  9, 25, 11)
  )

  final val map: Map[Int, RelayPins] = Map(
    12 -> default.replace(now =  4, before =  3),
    13 -> default.replace(now =  8, before = 11),
    17 -> default.replace(now =  4, before = 15),
    18 -> apply(left = Vector(2, 4, 14, 15, 17, 18), right = Vector(22, 23, 9, 24, 10, 8)),
    22 -> default.replace(now = 27, before = 18),
    24 -> default.replace(now = 27, before = 18)
  )   .withDefaultValue(default)

  private[this] lazy val _gpio = GpioFactory.getInstance

//  def gpio: GpioController = _gpio

  private[this] val gpioPins: Array[Pin] = {
      import RaspiPin._
    // bloody WiringPi has different counting system!
//    Array(
//      GPIO_00, GPIO_01, GPIO_02, GPIO_03, GPIO_04, GPIO_05, GPIO_06, GPIO_07,
//      GPIO_08, GPIO_09, GPIO_10, GPIO_11, GPIO_12, GPIO_13, GPIO_14, GPIO_15,
//      GPIO_16, GPIO_17, GPIO_18, GPIO_19, GPIO_20, GPIO_21, GPIO_22, GPIO_23,
//      GPIO_24, GPIO_25, GPIO_26, GPIO_27, GPIO_28, GPIO_29, GPIO_30, GPIO_31
//    )
    Array(
      /* 01 */ null   , /* 02 */ GPIO_08, /* 03 */ GPIO_09, /* 04 */ GPIO_07,
      /* 05 */ GPIO_21, /* 06 */ GPIO_22, /* 07 */ GPIO_11, /* 08 */ GPIO_10,
      /* 09 */ GPIO_13, /* 10 */ GPIO_12, /* 11 */ GPIO_14, /* 12 */ GPIO_26,
      /* 13 */ GPIO_23, /* 14 */ GPIO_15, /* 15 */ GPIO_16, /* 16 */ GPIO_27,
      /* 17 */ GPIO_00, /* 18 */ GPIO_01, /* 19 */ GPIO_24, /* 20 */ GPIO_28,
      /* 21 */ GPIO_29, /* 22 */ GPIO_03, /* 23 */ GPIO_04, /* 24 */ GPIO_05,
      /* 25 */ GPIO_06, /* 26 */ GPIO_25, /* 27 */ GPIO_02,
    )
  }

  private def mkPins(ids: Vec[Int]): Vec[GpioPinDigitalOutput] = {
    val io = _gpio
    ids.map { idx1 =>
      val idx = idx1 - 1
      io.provisionDigitalOutputPin(gpioPins(idx))
    }
  }

  private final val chanMap: Array[Array[Int]] = Array(
    Array(1, -1,  0, -1, -1,  1),
    Array(1, -1,  0, -1, -1,  0),
    Array(0,  1, -1, -1,  1, -1),
    Array(0,  1, -1, -1,  0, -1),
    Array(0,  0, -1,  1, -1, -1),
    Array(0,  0, -1,  0, -1, -1)
  )
}
final case class RelayPins(left: Vec[Int], right: Vec[Int]) {
  def replace(before: Int, now: Int): RelayPins = {
    val idxL = left.indexOf(before)
    if (idxL >= 0) copy(left = left.updated(idxL, now))
    else {
      val idxR = right.indexOf(before)
      if (idxR < 0) throw new IllegalArgumentException(s"Pin $before is not used in $this")
      copy(right = right.updated(idxR, now))
    }
  }

  lazy val both: Vec[Int] = left ++ right

  lazy val leftPins : Vec[GpioPinDigitalOutput] = RelayPins.mkPins(left )
  lazy val rightPins: Vec[GpioPinDigitalOutput] = RelayPins.mkPins(right)
  lazy val bothPins : Vec[GpioPinDigitalOutput] = leftPins ++ rightPins

  def selectChannel(ch: Int): Unit = {
    val m     = RelayPins.chanMap
    val pins  = if (ch < 6) leftPins else rightPins
    val mi    = 5 - (ch % 6)  // we had specified them in reverse order
    val arr   = m(mi)
    var i = 0
    while (i < 6) {
      val v = arr(i)
      if      (v == 0) pins(i).low ()  // .high()
      else if (v == 1) pins(i).high()  // .low ()
      i += 1
    }
  }
}