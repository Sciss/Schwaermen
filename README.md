# Schwärmen

This repository assembles different pieces and components of the transdisciplinary art project
[Schwärmen+Vernetzen](https://www.researchcatalogue.net/view/361990/361991).
Unless specified differently in the sub-projects, all code and materials (except text fragments, see below) are
(C)opyright 2017&ndash;2018 by Hanns Holger Rutz. All rights reserved. This project is released under the
[GNU General Public License](http://github.com/Sciss/ImperfectReconstruction/blob/master/LICENSE) v2+ and comes with absolutely no warranties.
To contact the author, send an email to `contact at sciss.de`.

All text fragments contained in the main project (C)opyright 2017 by Gertrude Grossegger.
All text contained in the catalog project (C)opyright 2018 by Hanns Holger Rutz.
All rights reserved.
No reuse or changes permitted without explicit written consent.

## building

All sub-projects build with sbt against Scala 2.12.

## running

To run a sound instance _from source_:

    sbt schwaermen-sound/run

From (control) laptop:

    sbt "schwaermen-sound/run --laptop"

To run the control interface:

    sbt schwaermen-control/run

To package sound deb:

    sbt schwaermen-sound/debian:packageBin
