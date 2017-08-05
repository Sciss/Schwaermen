# Schwärmen

This repository assembles different pieces and components of the transdisciplinary art project
[Schwärmen+Vernetzen](https://www.researchcatalogue.net/view/361990/361991).
Unless specified differently in the sub-projects, all code and materials are
(C)opyright 2017 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](http://github.com/Sciss/ImperfectReconstruction/blob/master/LICENSE) v2+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

## building

All sub-projects buildwith sbt 0.13 against Scala 2.12.

## running

To run a sound instance _from source_:

    sbt schwaermen-sound/run

From (control) laptop:

    sbt "schwaermen-sound/run --laptop"

To run the control interface:

    sbt schwaermen-control/run

To package sound deb:

    sbt schwaermen-sound/debian:packageBin
