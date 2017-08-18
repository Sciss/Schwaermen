/*
 *  package.scala
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

package de.sciss.negatum

import de.sciss.synth.ugen.{BinaryOpUGen, UnaryOpUGen}

package object impl {
//  type SynthGraphT = Topology[Vertex, Edge]

  type Vec[+A] = scala.collection.immutable.IndexedSeq[A]

  def graphElemName(in: Product): String =
    in match {
      case bin: BinaryOpUGen =>
        s"Bin_${bin.selector.id}"
      case un: UnaryOpUGen =>
        s"Un_${un.selector.id}"
      case _ => in.productPrefix
    }
}
