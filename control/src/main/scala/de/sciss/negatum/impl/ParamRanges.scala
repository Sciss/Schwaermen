package de.sciss.negatum.impl

import de.sciss.synth.ugen.BinaryOpUGen

import scala.language.implicitConversions

object ParamRanges {
  implicit def wrapOption[A](in: A): Option[A] = Some(in)
  implicit def wrapMargin(in: Double): Option[Margin] = Some(Margin(in))

  case class Margin(value: Double, hard: Boolean = true) {
    def soft: Boolean = !hard
  }

  // N.B. `lessThan` = technically less-than-or-equal
  case class Spec(lo: Option[Margin] = None, hi: Option[Margin] = None, dynamic: Boolean = false,
                  lessThan: Option[String] = None, scalar: Boolean = false)

  case class Info(params: Map[String, Spec] = Map.empty,
                  outLo: Option[Double] = None, outHi: Option[Double] = None,
                  dynamic: Option[Dynamic] = None)

  sealed trait Dynamic
  object Dynamic {
    case object Always extends Dynamic
    case class IfOver (param: String, lo: Double = 10.0) extends Dynamic
    case class IfUnder(param: String, hi: Double =  0.1) extends Dynamic
    case class And(elems: Dynamic*) extends Dynamic
    case class Or (elems: Dynamic*) extends Dynamic
    case class In (param: String  ) extends Dynamic
  }

  implicit def wrapDynamic(in: Boolean): Option[Dynamic] = if (in) Some(Dynamic.Always) else None
  def ifOver (param: String, lo: Double = 10.0): Option[Dynamic] = Some(Dynamic.IfOver (param, lo))
  def ifUnder(param: String, hi: Double =  0.1): Option[Dynamic] = Some(Dynamic.IfUnder(param, hi))

  val binOrDyn : Dynamic = Dynamic.Or (Dynamic.In("a"), Dynamic.In("b"))
  val binAndDyn: Dynamic = Dynamic.And(Dynamic.In("a"), Dynamic.In("b"))

  val map: Map[String, Info] = Map(
    // ---- Chaos ----
    "CuspN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "CuspL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    // FBSineN, FBSineL, FBSineC,
    "GbmanN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "GbmanL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "HenonN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "HenonL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "HenonC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "x0" -> Spec(scalar = true),
      "x1" -> Spec(scalar = true)
    )),
    "LatoocarfianN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LatoocarfianL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LatoocarfianC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "a" -> Spec(lo = -3.0, hi = +3.0),
      "b" -> Spec(lo = 0.5, hi = 1.5),
      "c" -> Spec(lo = 0.5, hi = 1.5),
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "LinCongN" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(  // outLo/Hi is a LIE
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LinCongL" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LinCongC" -> Info(dynamic = ifOver("freq"), /* outLo = -1.0, outHi = 1.0, */ params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "LorenzL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "h" -> Spec(lo = 0.0, hi = 0.06), // XXX TODO -- max h ?
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true),
      "zi" -> Spec(scalar = true)
    )),
    "QuadN" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "QuadL" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "QuadC" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq" -> Spec(),
      "xi" -> Spec(scalar = true)
    )),
    "StandardN" -> Info(dynamic = ifOver("freq"), params = Map(  // XXX TODO -- %2pi is a lie, with xi = 24 you get an initial value beyond that
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    "StandardL" -> Info(dynamic = ifOver("freq"), params = Map(  // XXX TODO -- %2pi is a lie, with xi = 24 you get an initial value beyond that
      "xi" -> Spec(scalar = true),
      "yi" -> Spec(scalar = true)
    )),
    // ---- Delay ----
    // ControlRate, SampleRate, SampleDur, ControlDur, SubsampleOffset, RadiansPerSample,
    // NumInputBuses, NumOutputBuses, NumAudioBuses, NumControlBuses,
    // NumBuffers, NumRunningSynths,
    // BufSampleRate, BufRateScale, BufSamples, BufFrames, BufChannels, BufDur,
    // PlayBuf, RecordBuf, BufRd, BufWr, Pitch,
    // BufDelayN, BufDelayL, BufDelayC, BufCombN, BufCombL, BufCombC,
    // BufAllpassN, BufAllpassL, BufAllpassC
    "DelayN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "DelayC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
    )),
    "CombN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "CombC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassN" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassL" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "AllpassC" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "maxDelayTime"  -> Spec(lo = 0.0, hi = 20.0, scalar = true),
      "delayTime"     -> Spec(lo = 0.0, lessThan = "maxDelayTime")
      // "decayTime"     -> Spec()
    )),
    "PitchShift" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "winSize"         -> Spec(lo = 0.001, hi = 2.0, scalar = true),  // arbitrary hi, !! lo > 0 !!
      "pitchRatio"      -> Spec(lo = 0.0, hi = 4.0),
      "pitchDispersion" -> Spec(lo = 0.0, hi = 1.0), // XXX TODO - hi ?
      "timeDispersion"  -> Spec(lo = 0.0, lessThan = "winSize")
    )),
    // TGrains, ScopeOut, ScopeOut2, Pluck, DelTapWr, DelTapRd, SetBuf, ClearBuf
    // ---- Demand ----
    // ---- DiskIO ----
    // ---- DynNoise ----
    "LFDNoise0" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDNoise1" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDNoise3" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    "LFDClipNoise" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq" -> Spec()
    )),
    // ---- FFT2 ----
    "RunningSum" -> Info(params = Map(
      "length" -> Spec(lo = 1.0, hi = 44100.0, scalar = true) // arbitrary hi, !!!! lo !!!!
    )),
    // ---- FFT ----
    // ---- Filter ----
    "Ramp" -> Info(dynamic = ifUnder("dur"), params = Map(
      "in" -> Spec(dynamic = true),
      "dur" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag2" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag3" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "LagUD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag2UD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "Lag3UD" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("timeUp", 1.0), Dynamic.IfUnder("timeUp", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "timeUp" -> Spec(lo = 0.0, hi = 30.0), // arbitrary hi
      "timeDown" -> Spec(lo = 0.0, hi = 30.0) // arbitrary hi
    )),
    "OnePole" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -0.999, hi = +0.999)
    )),
    "OneZero" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -1.0, hi = +1.0)
    )),
    "TwoPole" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0), // arbitrary
      "radius" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "TwoZero" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0), // arbitrary
      "radius" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "Decay" -> Info(dynamic = ifUnder("time", 1.0), params = Map(
      "in" -> Spec(dynamic = true),
      "time" -> Spec(lo = 0.0, hi = 30.0) // hi arbitrary
    )),
    "Decay2" -> Info(dynamic = Dynamic.And(Dynamic.IfUnder("attack", 1.0), Dynamic.IfUnder("release", 1.0)), params = Map(
      "in" -> Spec(dynamic = true),
      "attack" -> Spec(lo = 0.0, hi = 30.0), // hi arbitrary
      "release" -> Spec(lo = 0.0, hi = 30.0)  // hi arbitrary
    )),
    "Delay1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "Delay2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "Integrator" -> Info(dynamic = true /* XXX */, params = Map(
      "in" -> Spec(dynamic = true),
      "coeff" -> Spec(lo = -0.999, hi = +0.999)
    )),
    "LeakDC" -> Info(dynamic = true, params = Map(
      "coeff" -> Spec(lo = 0.8, hi = +0.99) // arbitrary
    )),
    "LPZ1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "HPZ1" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "LPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "HPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "BPZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    "BRZ2" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true)
    )),
    // APF
    "LPF" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "HPF" -> Info(dynamic = true, params = Map(
      "in" -> Spec(dynamic = true),
      "freq" -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "BPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "BRF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "RLPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),
    "RHPF" -> Info(dynamic = true, params = Map(
      "in"    -> Spec(dynamic = true),
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "rq"    -> Spec(lo = 0.01, hi = 100.0)    // arbitrary
    )),

    // TODO: continue here
    /*
    <ugen name="Slew">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal
            </doc>
        </arg>
        <arg name="up" default="1.0">
            <doc>
                maximum upward slope.
            </doc>
        </arg>
        <arg name="down" default="1.0">
            <doc>
                maximum downward slope.
            </doc>
        </arg>
        <doc>
            <text>
                A slew rate limiter UGen.
                Limits the slope of an input signal. The slope is expressed in units per second.

                Since the UGen is initialized with the initial value of the input signal, some tricks
                must be applied to set it to an alternative start value. For example:
                {{{
                val in = Select.kr(ToggleFF.kr(1), Seq("start".ir, "target".kr))
                Slew.kr(in)  // begins at "start" and moves towards "target"
                }}}
            </text>
        </doc>
    </ugen>
    <ugen name="Slope">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be measured
            </doc>
        </arg>
        <doc>
            <text>
                A UGen measuring the slope of signal.
                It calculates the rate of change per second of a signal, as given by the following formula:
                {{{
                out(i) = (in(i) - in(i-1)) * sampleRate
                }}}
                It thus equal to `HPZ1.ar(_) * 2 * SampleRate.ir`
            </text>
        </doc>
    </ugen>

    <ugen name="MidEQ">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="freq" default="440.0">
            <doc>
                center frequency in Hertz
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                reciprocal of Q. The Q (or quality) is conventionally defined as center-frequency / bandwidth,
                meaning that rq = bandwidth / center-frequency. A higher Q or lower rq produces a steeper filter.
                Too high values for `rq` may blow the filter up!
            </doc>
        </arg>
        <arg name="gain" default="0.0">
            <doc>
                The amount of boost (when positive) or attenuation (when negative) applied to
                the frequency band, in decibels.
            </doc>
        </arg>
        <doc>
            <text>
                A single band parametric equalizer UGen. It attenuates or boosts a frequency band.
            </text>
            <example name="mouse controlled frequency and boost">
                val in   = WhiteNoise.ar(0.25)
                val freq = MouseX.kr(200, 10000, 1)
                val gain = MouseY.kr(-12, 12) // bottom to top
                MidEQ.ar(in, freq, rq = 0.5, gain = gain)
            </example>
            <see>ugen.BPF</see>
            <see>ugen.BRF</see>
            <see>ugen.HPF</see>
            <see>ugen.LPF</see>
            <see>ugen.Resonz</see>
        </doc>
    </ugen>
    <ugen name="Median">
        <rate name="control"/>
        <rate name="audio">
            <arg name="in" rate="ugen">
                <doc>
                    input signal to be processed
                </doc>
            </arg>
        </rate>
        <arg name="length" default="3" init="true" pos="1">
            <doc>
                window size. I.e., the number of input samples in which to find the median.
                Must be an odd number from 1 to 31. A value of 1 has no effect.
                ''Warning'': This parameter is only read an initialization time and
                cannot be modulated while the UGen is running.
            </doc>
        </arg>
        <arg name="in" pos="0"/>
        <doc warn-pos="true">
            <text>
                A filter UGen that calculates the median of a running window over its input signal.
                This non-linear filter can be used to reduce impulse noise from a signal.
            </text>
            <example name="engage with mouse button">
                val in  = Saw.ar(500) * 0.1 + Dust2.ar(100) * 0.9 // signal plus noise
                val flt = Median.ar(in, 3)
                LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
            </example>
            <example name="long filter distorts by chopping off peaks in input">
                Median.ar(SinOsc.ar(1000) * 0.2, 31)
            </example>
            <see>ugen.LPF</see>
            <see>ugen.LeakDC</see>
            <see>ugen.RunningSum</see>
        </doc>
    </ugen>

    <ugen name="Resonz">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="freq" default="440.0">
            <doc>
                resonant frequency in Hertz
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                reciprocal of Q. The Q (or quality) is conventionally defined as center-frequency / bandwidth,
                meaning that rq = bandwidth / center-frequency. A higher Q or lower rq produces a steeper filter.
            </doc>
        </arg>
        <doc>
            <text>
                A two pole resonant filter UGen. It has zeroes at `z = +1` and `z = -1`.

                Based on K. Steiglitz, "A Note on Constant-Gain Digital Resonators", Computer Music Journal,
                vol 18, no. 4, pp. 8-10, Winter 1994.
            </text>
            <example name="modulated frequency">
                val in   = Saw.ar(200) * 0.5
                val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).madd(3600, 4000)
                Resonz.ar(in, freq)
            </example>
            <example name="mouse controlled frequency and Q">
                val in   = WhiteNoise.ar(0.5)
                val freq = MouseX.kr(200, 10000, 1)
                val q    = MouseY.kr(1, 100, 1) // bottom to top
                val flt  = Resonz.ar(in, freq, q.reciprocal)
                flt * q.sqrt // compensate for energy loss
            </example>
            <see>ugen.BPF</see>
            <see>ugen.Ringz</see>
            <see>ugen.HPF</see>
            <see>ugen.LPF</see>
            <see>ugen.MidEQ</see>
        </doc>
    </ugen>
    <ugen name="Ringz">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="freq" default="440.0">
            <doc>
                resonant frequency in Hertz
            </doc>
        </arg>
        <arg name="decay" default="1.0">
            <doc>
                the 60 dB decay time in seconds
            </doc>
        </arg>
        <doc>
            <text>
                A resonant or "ringing" filter UGen. This is the same as `Resonz`, except that instead of a
                Q parameter, the bandwidth is specified as a 60 dB ring decay time.
                One `Ringz` is equivalent to one component of the `Klank` UGen.
            </text>
            <example name="module ring time">
                Ringz.ar(Impulse.ar(6) * 0.3, 2000, XLine.kr(4, 0.04, 8))
            </example>
            <example name="modulated frequency">
                val in   = Saw.ar(200) * 0.02
                val freq = SinOsc.ar(XLine.ar(0.3, 100, 20)).madd(2800, 4800)
                Ringz.ar(in, freq)
            </example>
            <example name="multiple glissandi excited by noise">
                val ex = WhiteNoise.ar(0.001)
                Mix.fill(10) {
                  Ringz.ar(ex,
                    XLine.kr(ExpRand(100, 5000), ExpRand(100, 5000), 20),
                  0.5)
                }
            </example>
            <see>ugen.Resonz</see>
            <see>ugen.Formlet</see>
            <see>ugen.BPF</see>
            <see>ugen.Klank</see>
            <see>ugen.MidEQ</see>
        </doc>
    </ugen>
    <ugen name="Formlet">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="freq" default="440.0">
            <doc>
                resonant frequency in Hertz
            </doc>
        </arg>
        <arg name="attack" default="1.0">
            <doc>
                the 60 dB attack time in seconds
            </doc>
        </arg>
        <arg name="decay" default="1.0">
            <doc>
                the 60 dB decay time in seconds
            </doc>
        </arg>
        <doc>
            <text>
                A FOF-like resonant filter UGen. Its impulse response is like that of a sine wave with a `Decay2`
                envelope over it. It is possible to control the attack and decay times.

                `Formlet` is equivalent to:
                {{{
                Ringz(in, freq, decay) - Ringz(in, freq, attack)
                }}}

                The great advantage to this filter over FOF (Fonction d'onde formantique) is that there is no limit
                to the number of overlapping grains since the grain is just the impulse response of the filter.
            </text>
            <example name="modulated formant frequency">
                val in = Blip.ar(SinOsc.kr(5,0).madd(20, 300), 1000) * 0.1
                Formlet.ar(in, XLine.kr(1500, 700, 8), 0.005, 0.04)
            </example>
            <example name="mouse control of frequency and decay time">
                val in    = Blip.ar(SinOsc.kr(5,0).madd(20, 300), 1000) * 0.1
                val freq  = MouseY.kr(700, 2000, 1)
                val decay = MouseX.kr(0.01, 0.2, 1)
                Formlet.ar(in, freq, attack = 0.005, decay = decay)
            </example>
            <see>ugen.Ringz</see>
            <see>ugen.Resonz</see>
            <see>ugen.RLPF</see>
            <see>ugen.RHPF</see>
        </doc>
    </ugen>

    <ugen name="FOS">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="a0" default="0.0"/>
        <arg name="a1" default="0.0"/>
        <arg name="b1" default="0.0"/>
        <doc>
            <text>
                A first order filter section UGen. Filter coefficients are given directly rather
                than calculated for you. The formula is equivalent to:
                {{{
                out(i) = a0 * in(i) + a1 * in(i-1) + b1 * out(i-1)
                }}}
            </text>
            <example name="same as OnePole">
                val x = LFTri.ar(0.4) * 0.99
                FOS.ar(LFSaw.ar(200) * 0.1, 1 - x.abs, 0.0, x)
            </example>
            <example name="same as OneZero">
                val x = LFTri.ar(0.4) * 0.99
                FOS.ar(LFSaw.ar(200) * 0.1, 1 - x.abs, x, 0.0)
            </example>
            <see>ugen.SOS</see>
            <see>ugen.OnePole</see>
            <see>ugen.OneZero</see>
        </doc>
    </ugen>
    <ugen name="SOS">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be filtered
            </doc>
        </arg>
        <arg name="a0" default="0.0"/>
        <arg name="a1" default="0.0"/>
        <arg name="a2" default="0.0"/>
        <arg name="b1" default="0.0"/>
        <arg name="b2" default="0.0"/>
        <doc>
            <text>
                A second order filter section (biquad) UGen. Filter coefficients are given directly rather
                than calculated for you. The formula is equivalent to:
                {{{
                out(i) = a0 * in(i) + a1 * in(i-1) + a2 * in(i-2) + b1 * out(i-1) + b2 * out(i-2)
                }}}
            </text>
            <example name="same as TwoPole">
                val theta = MouseX.kr(0.2*math.Pi, 0.9*math.Pi)
                val rho   = MouseY.kr(0.6, 0.98)
                val b1    = 2.0 * rho * theta.cos
                val b2    = -(rho.squared)
                SOS.ar(WhiteNoise.ar(Seq(0.05, 0.05)), 1.0, 0.0, 0.0, b1, b2)
            </example>
            <example name="used as control signal">
                val theta = MouseX.kr(0.2*math.Pi, math.Pi)
                val rho   = MouseY.kr(0.6, 0.99)
                val b1    = 2.0 * rho * theta.cos
                val b2    = -(rho.squared)
                val vib   = SOS.kr(LFSaw.kr(3.16), 1.0, 0.0, 0.0, b1, b2)
                SinOsc.ar(vib * 200 + 600) * 0.2
            </example>
            <see>ugen.FOS</see>
        </doc>
    </ugen>

    <ugen name="Compander">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in">
            <doc>
                The signal to be compressed / expanded / gated.
            </doc>
        </arg>
        <arg name="ctrl">
            <doc>
                The signal whose amplitude controls the processor. Often the same as in, but one may wish
                to apply equalization or delay to it to change the compressor character (side-chaining), or even feed
                a completely different signal, for instance in a ducking application.
            </doc>
        </arg>
        <arg name="thresh" default="0.5">
            <doc>
                Control signal amplitude threshold, which determines the break point between slopeBelow
                and slopeAbove. Usually 0..1. The control signal amplitude is calculated using RMS.
            </doc>
        </arg>
        <arg name="ratioBelow" default="1.0">
            <doc>
                Slope of the amplitude curve below the threshold. If this slope &gt; 1.0, the amplitude
                will drop off more quickly the softer the control signal gets; when the control signal is close to 0
                amplitude, the output should be exactly zero -- hence, noise gating. Values &lt; 1.0 are possible,
                but it means that a very low-level control signal will cause the input signal to be amplified,
                which would raise the noise floor.
            </doc>
        </arg>
        <arg name="ratioAbove" default="1.0">
            <doc>
                Slope of the amplitude curve above the threshold. Values &lt; 1.0 achieve compression
                (louder signals are attenuated); &gt; 1.0, you get expansion (louder signals are made even louder).
                For 3:1 compression, you would use a value of 1/3 here.
            </doc>
        </arg>
        <arg name="attack" default="0.01">
            <doc>
                The amount of time it takes for the amplitude adjustment to kick in fully. This is
                usually pretty small, not much more than 10 milliseconds (the default value). I often set it as low as
                2 milliseconds (0.002).
            </doc>
        </arg>
        <arg name="release" default="0.1">
            <doc>
                The amount of time for the amplitude adjustment to be released. Usually a bit longer
                than attack; if both times are too short, you can get some (possibly unwanted) artifacts.
            </doc>
        </arg>
        <doc>
            <text>
                A compressor, expander, limiter, gate and ducking UGen. This dynamic processor uses a
                hard-knee characteristic. All of the thresholds and ratios are given as direct
                values, not in decibels!
            </text>
            <see>ugen.Limiter</see>
            <see>ugen.Normalizer</see>
        </doc>
    </ugen>
    <ugen name="Limiter">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in">
            <doc>
                input signal to be limited
            </doc>
        </arg>
        <arg name="level" default="1.0">
            <doc>
                maximum amplitude to which the signal is limited. The limiter will kick in
                when the input signal exceeds `+level` or falls below `-level`.
            </doc>
        </arg>
        <arg name="dur" default="0.01" init="true">
            <doc>
                look-ahead time in seconds
            </doc>
        </arg>
        <doc>
            <text>
                Limits the input amplitude to the given level. Unlike `Compander`, this UGen will never overshoot,
                but it needs to look ahead in the input signal, introducing a delay in its output.
                The delay time is equal to twice the value of the `dur` parameter (the buffer internally used).
            </text>
            <example name="compare dry and wet">
                val in = Decay2.ar(
                  Impulse.ar(8, phase = LFSaw.kr(0.25) * 0.7),
                  attack = 0.001, release = 0.3) * FSinOsc.ar(500)
                val flt = Limiter.ar(in, level = 0.4)
                LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
            </example>
            <see>ugen.Normalizer</see>
            <see>ugen.Compander</see>
        </doc>
    </ugen>
    <ugen name="Normalizer">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in">
            <doc>
                input signal to be normalized
            </doc>
        </arg>
        <arg name="level" default="1">
            <doc>
                peak output amplitude level to which to normalize the input
            </doc>
        </arg>
        <arg name="dur" default="0.01" init="true">
            <doc>
                look-ahead time in seconds. Shorter times will produce smaller delays and quicker
                transient response times, but may introduce amplitude modulation artifacts.
            </doc>
        </arg>
        <doc>
            <text>
                A UGen that normalizes the input amplitude to the given level.
                Unlike `Compander`, this UGen will not overshoot,
                but it needs to look ahead in the input signal, introducing a delay in its output.
                The delay time is equal to twice the value of the `dur` parameter (the buffer internally used).
            </text>
            <example name="compare dry and wet">
                val z    = Decay2.ar(
                  Impulse.ar(8, phase = LFSaw.kr(0.25) * 0.7),
                  attack = 0.001, release = 0.3) * FSinOsc.ar(500)
                val in  = z * SinOsc.ar(0.05) * 0.5
                val flt = Normalizer.ar(in, dur = 0.15, level = 0.4)
                LinXFade2.ar(in, flt, MouseButton.kr(-1, 1))
            </example>
            <see>ugen.Limiter</see>
            <see>ugen.Compander</see>
        </doc>
    </ugen>

    <ugen name="Amplitude">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be analyzed
            </doc>
        </arg>
        <arg name="attack" default="0.01">
            <doc>
                60 dB convergence time in for following attacks, in seconds
            </doc>
        </arg>
        <arg name="release" default="0.01">
            <doc>
                60 dB convergence time in for following decays, in seconds
            </doc>
        </arg>
        <doc>
            <text>
                An amplitude follower UGen. Tracks and reports the peak amplitude of its input signal.
            </text>
            <example name="use sound-card input to control pulse amplitude">
                // use headphones to prevent feedback!
                Pulse.ar(90, 0.3) * Amplitude.kr(PhysicalIn.ar(0))
            </example>
            <example name="compare with known amplitude">
                val amp = MouseX.kr
                val in  = PinkNoise.ar(amp)
                val ana = Amplitude.kr(amp, attack = 2, release = 2)
                (ana - amp).poll(2, "discrepancy")
                in
            </example>
            <see>ugen.DetectSilence</see>
        </doc>
    </ugen>
    <ugen name="DetectSilence" side-effect="true">   <!-- has done action, but does not set done flag -->
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be measured.
            </doc>
        </arg>
        <arg name="amp" default="0.0001" init="true">
            <doc>
                minimum amplitude threshold which must be exceeded for the input signal
                to be considered non-silent.
            </doc>
        </arg>
        <arg name="dur" default="0.1" init="true">
            <doc>
                The duration in seconds for which the input signal must be continuously smaller than or equal
                to the threshold to be considered silent.
            </doc>
        </arg>
        <arg name="doneAction" default="doNothing">
            <doc>
                an action to be performed when the output changes from zero to one (silence detected).
            </doc>
        </arg>
        <doc>
            <text>
                A UGen which detects whether its input signal falls below a given amplitude for a given amount
                of time (becoming "silent"). A silence is detected if the absolute sample values of the input
                remain less than or equal to the `amp` threshold for a consecutive amount of time given by the
                `dur` argument.

                A value of `1` is output when this condition is met, and a value of `0` is output when the
                condition is not met (i.e. at least one sample occurs in the input whose absolute value is
                greater than `amp`). Besides, when the output changes from zero to one, the `doneAction` is
                executed (unless it is `doNothing`).

                A special case is the initial condition of the UGen: It will begin with an output value of `0`
                (no silence detected), even if the input signal is below the amplitude threshold. It is only
                after the first input sample rising above the threshold that the actual monitoring begins and
                a trigger of `1` or the firing of the done-action may occur.
            </text>
            <see>ugen.Amplitude</see>
        </doc>
    </ugen>

    <ugen name="Hilbert">
        <output name="real"/>
        <output name="imag"/>
        <rate name="audio"/>
        <arg name="in">
            <doc>
                input signal to be processed
            </doc>
        </arg>
        <doc>
            <text>
                A Hilbert transform UGen. This transformation produces two signals from a given input with
                identical frequency content, but with their respective phases shifted to be 90 degrees apart
                (0.5 pi radians).

                The two signals output by `Hilbert` correspond to the real and imaginary part of the complex
                transformed signal. Due to the method used (an IIR filter), distortion occurs in the upper octave
                of the frequency spectrum.

                The transform can be used to implemented single-side-band (SSB) modulation, but a dedicated
                UGen `FreqShift` is already provided for this case.
            </text>
            <example name="a form of envelope tracking">
                val in = SinOsc.ar(440)
                val h  = Hilbert.ar(in)
                val x  = h.real.squared + h.imag.squared
                x.poll(1)  // cos(x)^2 + sin(x)^2 == 1 (ideally)
                0
            </example>
            <see>ugen.FreqShift</see>
        </doc>
    </ugen>
    <ugen name="FreqShift">
        <rate name="audio"/>
        <arg name="in"/>
        <arg name="freq" default="0.0">
            <doc>
                the shift amount in Hertz. Positive values shift upwards, negative values shift downwards.
            </doc>
        </arg>
        <arg name="phase" default="0.0"><!-- XXX TODO - what is this for? -->
            <doc>
                a phase parameter in radians (0 to 2 Pi).
            </doc>
        </arg>
        <doc>
            <text>
                A frequency shifting UGen. It implements single sideband (SSB) amplitude modulation, also known
                as frequency shifting, but not to be confused with pitch shifting. Frequency shifting moves all
                the components of a signal by a fixed amount but does not preserve the original harmonic
                relationships.
            </text>
            <example name="shift a sine frequency from 200 to 700 Hz">
                val freq = Line.ar(0, 500, 5)
                FreqShift.ar(SinOsc.ar(200) * 0.25, freq)
            </example>
            <example name="negative frequency to shift downwards">
                val freq = Line.ar(0, -500, 5)
                FreqShift.ar(SinOsc.ar(700) * 0.25, freq)
            </example>
            <see>ugen.Hilbert</see>
            <see>ugen.PV_MagShift</see>
        </doc>
    </ugen>
    <ugen name="MoogFF">
        <rate name="control"/>
        <rate name="audio"/>
        <arg name="in" rate="ugen"/>
        <arg name="freq" default="200.0">
            <doc>
                cutoff frequency in Hertz
            </doc>
        </arg>
        <arg name="gain" default="2.0">
            <doc>
                filter resonance gain, between 0 and 4
            </doc>
        </arg>
        <arg name="reset" default="closed">    <!-- strangely not a trigger, but a gate -->
            <doc>
                when greater than zero, this will reset the state of the digital filters at the beginning
                of the next control block.
            </doc>
        </arg>
        <doc>
            <text>
                A Moog VCF style UGen. This is a type of resonant low pass filter.

                The design of this filter is described in Federico Fontana, "Preserving the Digital Structure
                of the Moog VCF." In: Proceedings of the ICMC, Copenhagen 2007. Ported to SuperCollider by
                Dan Stowell.
            </text>
            <example name="mouse controlled">
                val in   = WhiteNoise.ar(01.1)
                val freq = MouseY.kr(100, 10000, 1)
                val gain = MouseX.kr(0, 4)
                Limiter.ar(MoogFF.ar(in, freq, gain))
            </example>
        </doc>
    </ugen>

    <ugen name="BLowPass">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                cutoff frequency.
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                the reciprocal of Q, hence bandwidth / cutoffFreq.
            </doc>
        </arg>
        <doc>
            <text>
                A 2nd order (12db per oct roll-off) resonant low pass filter UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BHiPass">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                cutoff frequency.
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                the reciprocal of Q, hence bandwidth / cutoffFreq.
            </doc>
        </arg>
        <doc>
            <text>
                A 2nd order (12db per oct roll-off) resonant high pass filter UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BBandPass">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                center frequency.
            </doc>
        </arg>
        <arg name="bw" default="1.0">
            <doc>
                the bandwidth '''in octaves''' between -3 dB frequencies
            </doc>
        </arg>
        <doc>
            <text>
                An band pass filter UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BBandStop">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                center frequency.
            </doc>
        </arg>
        <arg name="bw" default="1.0">
            <doc>
                the bandwidth '''in octaves''' between -3 dB frequencies
            </doc>
        </arg>
        <doc>
            <text>
                An band stop (reject) filter UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BPeakEQ">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                center frequency.
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                the reciprocal of Q, hence bandwidth / cutoffFreq.
            </doc>
        </arg>
        <arg name="gain" default="0.0">
            <doc>
                boost/cut at the center frequency (in decibels).
            </doc>
        </arg>
        <doc>
            <text>
                An parametric equalizer UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BAllPass">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                cutoff frequency.
            </doc>
        </arg>
        <arg name="rq" default="1.0">
            <doc>
                the reciprocal of Q, hence bandwidth / cutoffFreq.
            </doc>
        </arg>
        <doc>
            <text>
                An all pass filter UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BLowShelf">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                cutoff frequency.
            </doc>
        </arg>
        <arg name="rs" default="1.0">
            <doc>
                the reciprocal of the slope S (Shell boost/cut slope).
                When `S = 1`, the shelf slope is as steep as it can be and remain monotonically increasing
                or decreasing gain with frequency.  The shelf slope, in dB/octave, remains proportional to
                S for all other values for a fixed freq/sample-rate and `gain`.
            </doc>
        </arg>
        <arg name="gain" default="0.0">
            <doc>
                boost/cut at the cutoff frequency (in decibels).
            </doc>
        </arg>
        <doc>
            <text>
                A low shelf equalizer UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
    <ugen name="BHiShelf">
        <rate name="audio" implied="true"/>
        <arg name="in" rate="ugen">
            <doc>
                input signal to be processed.
            </doc>
        </arg>
        <arg name="freq" default="500.0">
            <doc>
                cutoff frequency.
            </doc>
        </arg>
        <arg name="rs" default="1.0">
            <doc>
                the reciprocal of the slope S (Shell boost/cut slope).
                When `S = 1`, the shelf slope is as steep as it can be and remain monotonically increasing
                or decreasing gain with frequency.  The shelf slope, in dB/octave, remains proportional to
                S for all other values for a fixed freq/sample-rate and `gain`.
            </doc>
        </arg>
        <arg name="gain" default="0.0">
            <doc>
                boost/cut at the cutoff frequency (in decibels).
            </doc>
        </arg>
        <doc>
            <text>
                A high shelf equalizer UGen.
                The B equalization suite is based on the Second Order Section (SOS) biquad UGen.

                Note: Biquad coefficient calculations imply certain amount of CPU overhead. These
                plugin UGens contain optimizations such that the coefficients get updated only when
                there has been a change to one of the filter's parameters. This can cause spikes in
                CPU performance and should be considered when using several of these units.
            </text>
        </doc>
    </ugen>
         */

    // ---- Gendyn ---- TODO
    // ---- Grain ----
    // ---- IO ----
    // ---- Keyboard ----

    // ---- LF ----
    //  Vibrato
    "LFPulse" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "LFSaw" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = -1.0, hi = 1.0, scalar = true),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),
    "LFPar" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true)
    )),
    "LFCub" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 1.0, scalar = true)
    )),
    "LFTri" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "iphase" -> Spec(lo = 0.0, hi = 4.0, scalar = true)
    )),
    "LFGauss" -> Info(dynamic = ifUnder("dur", 0.1), params = Map(
      "dur"     -> Spec(lo = 5.0e-5, hi = 100.0),
      "width"   -> Spec(lo = 0.0, hi = 1.0),
      "phase"   -> Spec(lo = 0.0, hi = 1.0)  // XXX TODO -- hi correct?
    )),
    "Impulse" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"    -> Spec(lo = 0.1, hi = 20000.0),
      "phase"   -> Spec(lo = 0.0, hi = 1.0)
    )),
    "VarSaw" -> Info(dynamic = ifOver("freq"), params = Map(
      "freq"    -> Spec(lo = 0.01, hi = 20000.0),
      "iphase"  -> Spec(lo = 0.0, hi = 1.0, scalar = true),
      "width"   -> Spec(lo = 0.0, hi = 1.0)
    )),
    "SyncSaw" -> Info(dynamic = Dynamic.Or(Dynamic.IfOver("syncFreq"), Dynamic.IfOver("sawFreq")), params = Map(
      "syncFreq"  -> Spec(lo = 0.01, hi = 20000.0),
      "sawFreq"   -> Spec(lo = 0.01, hi = 20000.0)
    )),
    // K2A, A2K, T2K, T2A, DC

    // TODO: continue here
    /*

      <ugen name="Line" side-effect="true" done-flag="true">
          <rate name="audio"/>
          <rate name="control"/>
          <arg name="start" default="0.0" init="true">
              <doc>
                  Starting value
              </doc>
          </arg>
          <arg name="end" default="1.0" init="true">
              <doc>
                  Ending value
              </doc>
          </arg>
          <arg name="dur" default="1.0" init="true">
              <doc>
                  Duration in seconds
              </doc>
          </arg>
          <arg name="doneAction" default="doNothing">
              <doc>
                  A done-action that is evaluated when the Line has reached the end value after the
                  given duration
              </doc>
          </arg>
          <doc>
              <text>
                  A line generator UGen that moves from a start value to the end value in a given duration.
              </text>
              <example name="pan from left to right">
                  Pan2.ar(PinkNoise.ar(0.3), Line.kr(-1, 1, 10, freeSelf))
              </example>
              <see>ugen.XLine</see>
              <see>ugen.EnvGen</see>
              <see>ugen.Ramp</see>
          </doc>
      </ugen>
      <ugen name="XLine" side-effect="true" done-flag="true">
          <rate name="audio"/>
          <rate name="control"/>
          <arg name="start" default="1.0" init="true">
              <doc>
                  Starting value
              </doc>
          </arg>
          <arg name="end" default="2.0" init="true">
              <doc>
                  Ending value
              </doc>
          </arg>
          <arg name="dur" default="1.0" init="true">
              <doc>
                  Duration in seconds
              </doc>
          </arg>
          <arg name="doneAction" default="doNothing">
              <doc>
                  A done-action that is evaluated when the `Line` has reached the end value after the
                  given duration
              </doc>
          </arg>
          <doc>
              <text>
                  An exponential curve generator UGen that moves from a start value to the end value in a given duration.

                  At a given point in time `0 &lt;= t &lt;= dur`, the output value is `start * (stop/start).pow(t/dur)`.

                  '''Warning''': It must be ensured that the both `start` is not zero and `start` and `end` have the
                  same sign (e.g. a `start` of `-1` and an end of `-0.001` are valid),
                  otherwise the UGen will output a `NaN`! While in the case of `end` being zero the UGen will also
                  output zero, it is recommended to treat this case as pathological as well.
              </text>
              <example name="glissando">
                  SinOsc.ar(Line.kr(200, 2000, 10, freeSelf)) * 0.2
              </example>
              <see>ugen.Line</see>
          </doc>
      </ugen>

      <ugen name="Wrap">
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="in">
              <doc>
                  input signal to constrain
              </doc>
          </arg>
          <arg name="lo" default="0.0">
              <doc>
                  lower margin of wrapping (inclusive)
              </doc>
          </arg>
          <arg name="hi" default="1.0">
              <doc>
                  upper margin of wrapping (exclusive)
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen that constrains a signal to a given range, by
                  "wrapping" values outside the range. This is similar to
                  the `wrap2` binary operator but permits both a lower
                  range value `lo` and an upper range value `hi`.

                  An input value greater than or equal to `hi` will be wrapped
                  back to `(in - hi) % (hi - lo) + lo`. An input value less than
                  `lo` will be wrapped back to `hi - (lo - in) % (hi - lo)`.
              </text>
              <example name="wrap pulse wave to modulate timbre">
                  val hi = SinOsc.ar(0.1).linexp(-1, 1, 0.01, 1.0)
                  Wrap.ar(Pulse.ar(300), 0, hi) * 0.2 / hi
              </example>
              <see>ugen.Fold</see>
              <see>ugen.Clip</see>
          </doc>
      </ugen>
      <ugen name="Fold">
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="in">
              <doc>
                  input signal to constrain
              </doc>
          </arg>
          <arg name="lo" default="0.0">
              <doc>
                  lower margin of folding
              </doc>
          </arg>
          <arg name="hi" default="1.0">
              <doc>
                  upper margin of folding
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen that constrains a signal to a given range, by
                  "folding" values outside the range. This is similar to
                  the `fold2` binary operator but permits both a lower
                  range value `lo` and an upper range value `hi`.

                  Folding can be understood as "reflecting" around the boundaries.
                  For example, if the upper margin is 3, then if an input value
                  exceeds 3, the excess is negatively reflected; 3.1 becomes 2.9,
                  3.2 becomes 2.8, etc. until the lower margin is reached again
                  where another reflection occurs. Likewise, if the lower margin
                  is 1, then if an input value falls below 1, the undershoot is
                  reflected; 0.9 becomes 1.1, 0.8 becomes 1.2, etc. until the upper
                  margin is reached again where another reflection occurs.
              </text>
              <example name="fold sawtooth wave to modulate timbre">
                  val hi = SinOsc.ar(0.1).linexp(-1, 1, 0.01, 1.0)
                  Fold.ar(Saw.ar(300), 0, hi) * 0.2 / hi
              </example>
              <see>ugen.Wrap</see>
              <see>ugen.Clip</see>
          </doc>
      </ugen>
      <ugen name="Clip">
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="in">
              <doc>
                  input signal to constrain
              </doc>
          </arg>
          <arg name="lo" default="0.0">
              <doc>
                  lower margin of clipping
              </doc>
          </arg>
          <arg name="hi" default="1.0">
              <doc>
                  upper margin of clipping
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen that constrains a signal to a given range, by
                  limiting values outside the range to the range margins. This is similar to
                  the `clip2` binary operator but permits both a lower
                  range value `lo` and an upper range value `hi`.

                  Mathematically, this is equivalent to `in.max(lo).min(hi)`.
              </text>
              <example name="clip sine wave to modulate timbre">
                  val hi = SinOsc.ar(0.1).linexp(-1, 1, 0.01, 1.0)
                  Clip.ar(SinOsc.ar(300), 0, hi) * 0.2 / hi
              </example>
              <see>ugen.Wrap</see>
              <see>ugen.Fold</see>
          </doc>
      </ugen>

      // AmpComp, AmpCompA

      <ugen name="InRange">
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="in">
              <doc>
                  input signal to test
              </doc>
          </arg>
          <arg name="lo" default="0.0">
              <doc>
                  lower margin of test range (inclusive)
              </doc>
          </arg>
          <arg name="hi" default="1.0">
              <doc>
                  upper margin of test range (inclusive)
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen that tests if a signal is within a given range. If `in &gt;= lo` and `in &lt;= hi`, outputs 1.0,
                  otherwise outputs 0.0.
              </text>
              <example name="detect whether mouse is in specific horizontal range">
                  val x = MouseX.kr
                  InRange.kr(x, 0.4, 0.6) * PinkNoise.ar(0.3)
              </example>
              <see>ugen.InRect</see>
              <see>ugen.Clip</see>
              <see>ugen.Schmidt</see>
          </doc>
      </ugen>
      <ugen name="InRect">
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="x">
              <doc>
                  "horizontal" signal to test
              </doc>
          </arg>
          <arg name="y">
              <doc>
                  "vertical" signal to test
              </doc>
          </arg>
          <arg name="left" default="0.0">
              <doc>
                  lower margin of horizontal test range (inclusive)
              </doc>
          </arg>
          <arg name="top" default="0.0">
              <doc>
                  lower margin of vertical test range (inclusive)
              </doc>
          </arg>
          <arg name="right" default="1.0">
              <doc>
                  upper margin of horizontal test range (inclusive)
              </doc>
          </arg>
          <arg name="bottom" default="1.0">
              <doc>
                  upper margin of vertical test range (inclusive)
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen that tests if two signals lie both within a given ranges. The two input signals can be
                  understood as horizontal and vertical coordinates, therefore the test become one that
                  determines whether the input is within a given "rectangle".

                  If `x &gt;= left` and `x &lt;= right` and `y &gt; top` and `y &lt;= bottom`, outputs 1.0,
                  otherwise outputs 0.0.
              </text>
              <example name="detect whether mouse is in specific horizontal and vertical range">
                  val x  = MouseX.kr; val y = MouseY.kr(1, 0)
                  val in = InRect.kr(x = x, y = y, left = 0.4, top = 0.2, right = 0.6, bottom = 0.4)
                  in * PinkNoise.ar(0.3)
              </example>
              <see>ugen.InRange</see>
              <see>ugen.Clip</see>
          </doc>
      </ugen>
      <ugen name="LinExp"> <!-- XXX TODO: first arg rate has to be rate ? -->
          <rate name="scalar"/>
          <rate name="control"/>
          <rate name="audio"/>
          <arg name="in" rate="ugen">
              <doc>
                  input signal to convert
              </doc>
          </arg>
          <arg name="srcLo" default="0.0">
              <doc>
                  lower limit of input range
              </doc>
          </arg>
          <arg name="srcHi" default="1.0">
              <doc>
                  upper limit of input range
              </doc>
          </arg>
          <arg name="dstLo" default="1.0">
              <doc>
                  lower limit of output range
              </doc>
          </arg>
          <arg name="dstHi" default="2.0">
              <doc>
                  upper limit of output range
              </doc>
          </arg>
          <doc>
              <text>
                  A UGen which maps a linear range to an exponential range.
                  The equivalent formula is `(dstHi / dstLo).pow((in - srcLo) / (srcHi - srcLo)) * dstLo`.

                  '''Note''': No clipping is performed. If the input signal exceeds the input range,
                  the output will also exceed its range.
              </text>
              <example name="translate linear noise into exponential frequencies">
                  val mod = LFNoise2.ar(10)
                  val lo  = MouseX.kr(200, 8000, 1)
                  val hi  = MouseY.kr(200, 8000, 1)
                  SinOsc.ar(LinExp.ar(mod, -1, 1, lo, hi)) * 0.1
              </example>
              <see>ugen.LinExp</see>
              <see>ugen.Clip</see>
          </doc>
      </ugen>

      // EnvGen, Linen, IEnvGen

     */

    s"Bin_${BinaryOpUGen.Plus     .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Minus    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Times    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Mod      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Eq       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Neq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Lt       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Gt       .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Leq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Geq      .id}" -> Info(outLo = 0.0, outHi = 1.0),
    s"Bin_${BinaryOpUGen.Min      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Max      .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.BitAnd   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.BitOr    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.BitXor   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.RoundTo  .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.RoundUpTo.id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Trunc    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Atan2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Hypot    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Hypotx   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Pow      .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring1    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring3    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Ring4    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Difsqr   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sumsqr   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sqrsum   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Sqrdif   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Absdif   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Thresh   .id}" -> Info(dynamic = binAndDyn),
    s"Bin_${BinaryOpUGen.Amclip   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Scaleneg .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Clip2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Excess   .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Fold2    .id}" -> Info(dynamic = binOrDyn),
    s"Bin_${BinaryOpUGen.Wrap2    .id}" -> Info(dynamic = binOrDyn),
    // Div,

    // ---- OSC ----
    // DegreeToKey, Select, TWindex, Index, IndexL, FoldIndex, WrapIndex,
    // IndexInBetween, DetectIndex, Shaper,
    // FSinOsc, SinOscFB, VOsc, VOsc3, Osc, OscN, COsc,
    // Klang, Klank

    // XXX TODO: Blip, Saw

    "SinOsc" -> Info(dynamic = ifOver("freq"), outLo = -1.0, outHi = +1.0, params = Map(
      "freq"  -> Spec(lo = 0.01, hi = 20000.0),
      "phase" -> Spec()  // wraps around 2pi
    )),
    //    "Formant" -> Info(dynamic = true, params = Map(
    //      "fundFreq"  -> Spec(lo = 10.0, hi = 18000.0), // XXX TODO -- not sure this accepts very low frequencies
    //      "formFreq"  -> Spec(lo = 10.0, hi = 18000.0),
    //      "bw"        -> Spec(lo = 10.0, hi =  4000.0, greaterThan = "fundFreq")
    //    )),

    "Blip" -> Info(dynamic = true, outLo = -1.0, outHi = +1.0, params = Map(
      "freq"    -> Spec(lo = 10.0, hi = 20000.0),
      "numHarm" -> Spec(lo = 1.0)
    )),
    "Saw" -> Info(dynamic = true, params = Map( // Unfortunately can seriously exceed outLo = -1.0, outHi = +1.0
      "freq"    -> Spec(lo = 10.0, hi = 20000.0)
    )),
    "Pulse" -> Info(dynamic = true, params = Map( // Unfortunately can seriously exceed outLo = -1.0, outHi = +1.0
      "freq"  -> Spec(lo = 10.0, hi = 20000.0),
      "width" -> Spec(lo = 0.0, hi = 1.0)
    )),

    "GVerb" -> Info(dynamic = true, params = Map(
      "in"            -> Spec(dynamic = true),
      "roomSize"      -> Spec(lo = 0.55, lessThan = "maxRoomSize"),  // lo!
      "revTime"       -> Spec(lo = 0.0, hi = 100.0 /* soft */),
      "damping"       -> Spec(lo = 0.0, hi = 1.0),
      "inputBW"       -> Spec(lo = 0.0, hi = 1.0),
      "spread"        -> Spec(lo = 0.0, hi = 43.0), // hi!
      // "dryLevel"      -> Spec(),
      // "earlyRefLevel" -> Spec(),
      // "tailLevel"     -> Spec(),
      "maxRoomSize"   -> Spec(lo = 0.55, hi = 300.0 /* soft */, scalar = true)
    ))
  )
}
