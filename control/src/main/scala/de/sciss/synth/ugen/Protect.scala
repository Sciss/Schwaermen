/*
 *  Protect.scala
 *  (Negatum)
 *
 *  Copyright (c) 2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth
package ugen

import de.sciss.negatum.impl.{MkSynthGraph, ParamRanges, graphElemName}

final case class Protect(in: GE, lo: Double, hi: Double, dynamic: Boolean) extends GE.Lazy {
  def rate: MaybeRate = in.rate

  protected def makeUGens: UGenInLike = {
    val inInfoOpt = ParamRanges.map.get(graphElemName(in))

    lazy val inLoOpt: Option[Double] = in match {
      case Constant(f) => Some(f.toDouble)
      case _ => inInfoOpt.flatMap(_.outLo)
    }
    lazy val inHiOpt: Option[Double] = in match {
      case Constant(f) => Some(f.toDouble)
      case _ => inInfoOpt.flatMap(_.outHi)
    }
    val loThresh  = lo // pSpec.lo.fold(Double.NegativeInfinity)(_.value)
    val hiThresh  = hi // pSpec.hi.fold(Double.PositiveInfinity)(_.value)
    val loOk      = inLoOpt.exists(_ >= loThresh)
    val hiOk      = inHiOpt.exists(_ <= hiThresh)

    val inGE1: GE = (loOk, hiOk) match {
      case (false, _ ) if !loThresh.isInfinity && (hiOk || hiThresh.isInfinity) =>
        in.max(loThresh)
      case (_ , false) if !hiThresh.isInfinity && (loOk || loThresh.isInfinity) =>
        in.min(hiThresh)
      case (false, false) if !hiThresh.isInfinity && !loThresh.isInfinity =>
        // N.B. Clip.ar seems to be broken
        // inGE0.clip(loThresh, hiThresh)
        in.max(loThresh).min(hiThresh)
      case _ => in
    }
    // `lessThan` -> see below

    val inGE2: GE = if (!dynamic || MkSynthGraph.isDynamic(inGE1)) inGE1 else LeakDC.ar(inGE1)
    inGE2
  }
}
