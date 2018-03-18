/*
 *  CatalogGUI.scala
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

object CatalogGUI {
  /*

    ok, here is the idea:

    - for each possible page-folding configuration:
    - create a GNG from the possible space occupied by the edges
      around the paragraphs
    - turn it into a MST
    - locate the possible starting and stopping points
    - find the path
    - create a bezier along it
    - make sure we don't overlap the paragraphs, otherwise
      rerun with stronger boundary padding
    - render the edge text to svg
    - create a place-on-path version of it
    - render it to PDF(s)

   */
}
