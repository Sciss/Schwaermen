package de.sciss.negatum.impl

import de.sciss.negatum.impl.ParamRanges.Dynamic
import de.sciss.synth.GE
import de.sciss.synth.ugen.Constant

object MkSynthGraph {
//  final case class Incomplete(in: SynthGraphT, vertex: Vertex.UGen, arg: String, argList: Seq[String])
//    extends Exception {
//
//    override def toString = s"$productPrefix(\n  in = $in,\n  vertex = $vertex,\n  arg = $arg\n,  argList = $argList\n)"
//  }

  def isDynamic(in: GE): Boolean = in match {
    case Constant(_) => false
    case _ =>
      ParamRanges.map.get(graphElemName(in)).exists { info =>
        def getArg(name: String): Any = in.getClass.getMethod(name).invoke(in)

        def check(d: Dynamic): Boolean = d match {
          case Dynamic.Always => true
          case Dynamic.And(elems @ _*) => elems.forall(check)
          case Dynamic.Or (elems @ _*) => elems.exists(check)
          case Dynamic.IfOver (param, v) =>
            getArg(param) match {
              case Constant(f) if f >= v => true
              case _ => false // XXX TODO --- could be more complex
            }
          case Dynamic.IfUnder(param, v) =>
            getArg(param) match {
              case Constant(f) if f <= v => true
              case _ => false // XXX TODO --- could be more complex
            }
          case Dynamic.In(param) =>
            getArg(param) match {
              case argGE: GE => isDynamic(argGE)
              case _ => false
            }
        }

        info.dynamic.exists(check)
      }
  }
}
