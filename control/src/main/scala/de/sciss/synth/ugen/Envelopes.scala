/*
 *  Envelopes.scala
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

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.language.implicitConversions

trait EnvGenCompanion extends Product {
  protected implicit def floatArg(f: Float): UGenSpec.ArgumentValue.Float =
    UGenSpec.ArgumentValue.Float(f)

  protected implicit def intArg(i: Int): UGenSpec.ArgumentValue.Int =
    UGenSpec.ArgumentValue.Int(i)

  def envelopeArgs: Vec[UGenSpec.Argument]
}

trait EnvGenLike extends GE.Lazy with HasDoneFlag with AudioRated {
  def gate: GE
  def levelScale: GE
  def levelBias: GE
  def timeScale: GE

  protected def mkEnv: Env

  final protected def makeUGens: UGenInLike =
    EnvGen.ar(mkEnv, gate = gate, levelScale = levelScale, levelBias = levelBias, timeScale = timeScale)
}

case object EnvGen_ADSR extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("attack"      , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("decay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.3f), rates = Map.empty),
    Argument("sustainLevel", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.5f), rates = Map.empty),
    Argument("release"     , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("peakLevel"   , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_ADSR(attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f, release: GE = 1.0f,
                             peakLevel: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.adsr(attack = attack, decay = decay, sustainLevel = sustainLevel, release = release /* , curve = curve */)
}

case object EnvGen_ASR extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_ASR(attack: GE = 0.01f, level: GE = 1.0f, release: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                            gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.asr(attack = attack, level = level, release = release /*, curve = curve */)
}

case object EnvGen_CutOff extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.1f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_CutOff(release: GE = 0.1f, level: GE = 1.0f, /* curve: Env.Curve = linear, */
                               gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.cutoff(release = release, level = level /* , curve = curve */)
}

case object EnvGen_DADSR extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("delay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.1f), rates = Map.empty),
    Argument("attack"      , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("decay"       , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.3f), rates = Map.empty),
    Argument("sustainLevel", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.5f), rates = Map.empty),
    Argument("release"     , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("peakLevel"   , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_DADSR(delay: GE = 0.1f, attack: GE = 0.01f, decay: GE = 0.3f, sustainLevel: GE = 0.5f,
                              release: GE = 1.0f, peakLevel: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                              gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.dadsr(delay = delay, attack = attack, decay = decay, sustainLevel = sustainLevel,
      release = release /*, curve = curve */)
}

case object EnvGen_Linen extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("sustain", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_Linen(attack: GE = 0.01f, sustain: GE = 1.0f, release: GE = 1.0f, level: GE = 1.0f,
                              /* curve: Env.Curve = linear, */
                              gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.linen(attack = attack, sustain = sustain, release = release, level = level /*, curve = curve */)
}

case object EnvGen_Perc extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[UGenSpec.Argument](
    Argument("attack" , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 0.01f),rates = Map.empty),
    Argument("release", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty),
    Argument("level"  , tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1.0f), rates = Map.empty)
  )
}
final case class EnvGen_Perc(attack: GE = 0.01f, release: GE = 1.0f, level: GE = 1.0f, /* curve: Env.Curve = parametric(-4), */
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.perc(attack = attack, release = release, level = level /* , curve = curve */)
}

case object EnvGen_Sine extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[Argument](
    Argument("dur", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty),
    Argument("level", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty)
  )
}
final case class EnvGen_Sine(dur: GE = 1.0f, level: GE = 1.0f,
                             gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.sine(dur = dur, level = level)
}

case object EnvGen_Triangle extends EnvGenCompanion {
  import UGenSpec._
  val envelopeArgs = Vec[Argument](
    Argument("dur", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty),
    Argument("level", tpe = ArgumentType.GE(SignalShape.Generic), defaults = Map(UndefinedRate -> 1f), rates = Map.empty)
  )
}
final case class EnvGen_Triangle(dur: GE = 1.0f, level: GE = 1.0f,
                                 gate: GE = 1, levelScale: GE = 1.0f, levelBias: GE = 0.0f, timeScale: GE = 1.0f)
  extends EnvGenLike {

  protected def mkEnv: Env =
    Env.triangle(dur = dur, level = level)
}
