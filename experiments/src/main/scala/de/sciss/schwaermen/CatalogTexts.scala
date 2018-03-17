/*
 *  CatalogTexts.scala
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

import scala.collection.immutable.{Seq => ISeq}

object CatalogTexts {

  private def mkText(in: String): String = {
    val s0 = in.stripMargin
    val s1 = s0.replace('\n', ' ')
    val s2 = s1.replace("  ", " ")
    s2.trim
  }

  val parDe: ISeq[String] = List(
    """Die Gassen sind eng und mit starkem Gefälle hinunter zum Wasser. Ich gehe noch einmal zurück.
      |Alles ist von einer starken Vertikalität geprägt. Ich ziehe mich an einer Wand hoch, an Seilen
      |und Griffmöglichkeiten. Ist das der einzige Weg oder nur eine Abkürzung? Missbilligung bei den
      |Umstehenden? Eine Ecke des Raumes ist aus Glas, vom Boden zur Decke, ein riesiges Fenster nach
      |draußen, orangefarbenes Licht fällt herein. Jemand ruft mich an, er und seine Partnerin hätten
      |Schwierigkeiten gehabt, die Installation in der Ausstellung zu verstehen. Dort sind
      |vibrierende, sich mit hoher Frequenz und geringer Amplitude schüttelnde Dinge montiert auf
      |einem Feld oder Tisch, von der Größe kleiner Äpfel.
      |""",
    """Es handelt sich um ein Experiment. Zwei menschliche Arme werden zusammengelegt, nebeneinander,
      |dicht beieinander. Sie werden gequetscht oder seziert, geschnitten, dann vereint auf einem
      |Objektträger oder Glas. Ein Exemplar wird hergestellt, der \emph{Ausschnitt} des doppelten
      |Arms wird zu einem Objekt. Was passiert mit dem Leben? Das Leben war ja vormalig separat in
      |jedem der Arme. Inwieweit vereint sich das Leben in den zusammengequetschten Armen, wo es doch
      |auf zwei unterschiedliche Linien zurückverfolgt werden kann? Tim Ingold erinnert an Deleuze's
      |Verwendung der Haecceitas oder des \emph{Bündels} von Linien zur Bezeichnung des lebendigen
      |Organismus. Die Traurigkeit der Unwiederholbarkeit: In einer Erfahrung vereint, die jedoch nie
      |stattfand, ausser in der Erinnerung.
      |""",
    """Ein Plan liegt vor mir oder in meinem Gedächtnis. Ein Glas zerspringt auf dem Boden, doch
      |lässt es sich wenig später wieder in ein Metall einfassen. Die Karte hat einen kleinen
      |Maßstab, ich verfolge den Weg im Geiste oder mit dem Finger. Länger, als ich ursprünglich
      |dachte, viel länger. Ich sehe alles nah, jedoch von oben. Es ist zu weit, in vielleicht einer
      |Stunde wird es dunkel sein, dann wird es schwierig bis unmöglich, dem Weg zu folgen. Eine
      |Falle taucht auf. Unbemerkbar auf der Karte mit ihrer Vogelperspektive erscheint eine Stelle,
      |in der der Weg steil abfällt, es wird immer steiler bis es teilweise senkrecht bergab geht.
      |""",
    """Ein Funktionsgenerator aus der Frühzeit, ein Ding mit flachem Bildschirm, etwa in der Größe 40
      |mal 30 Zentimeter. Der Hintergrund des Schirms ist blau, darauf Strichzeichnung in weiß.
      |Funktionen, geometrische Funktionen, Linien und Kreise, Plots. Sie schwimmen umher, vielleicht
      |durch Interaktion bewegt, und ergeben ein Ensemble von Formen. Mir kommt der Gedanke, dass wir
      |nur endliche Energie haben. Die Formen geben synthetische Klänge von sich ab, die sich
      |zusammenmischen zu einem merkwürdigen Gesamtbild. Heutzutage keine überraschenden Klänge, aber
      |man kann sich leicht vorstellen, wie bei der Ankunft des Generators alle fasziniert waren von
      |der Reinheit der Klänge und von deren eigenwilligem und leicht chaotischem Erscheinen,
      |Vorübergehen und Zusammengehen.
      |""",
    """Ein eher dunkler Raum, in welchem jemand beginnen würde eine Lichtquelle, ein rotes Licht, zu
      |rotieren und regelmäßige Kreise zu erzeugen, welche sich dann im Raum bewegten (der Performer
      |begann sich im Raum zu bewegen). Ich schien mich zu erinnern: ja, alles war rot. Nein. In der
      |Tat änderte sich die Farbe des Lichtes. Die Arbeiten nahmen die Besucher mit herum im Raum,
      |sie mussten zu rennen anfangen und wurden gleichermaßen zu Performern und zum Performten.
      |Alles beschleunigte sich, so dass die Leute kaum mithalten konnten, doch selbst Gruppen
      |älterer Menschen, einige mit Krücken, hetzten als Gefolge umher. Ich wollte dem sinnlosen
      |Treiben nicht beiwohnen und ging hinaus.
      |""",
    """Wir stehen über eine Graphik gebeugt, ein Diagramm. In ihm überlappen sich eine Zellstruktur,
      |eine organisch geschlungene Zeichnung, und ein rigides, gitterförmiges Gefüge wie eine
      |Gruppierung von Schaltkreisen. Wir sind dabei zu formulieren, was uns an dieser Struktur
      |gefällt. Zum einen lassen sich rechnende Funktionen ablagern, in die Hand nehmen und auf dem
      |Gitterblatt einsetzen, so dass jedes Element der Gitterebene als Empfänger oder Platzhalter
      |einer rechnenden Funktion erscheint. Zum anderen produzierte die geschlungene Zellebene einen
      |Querschnitt, ein zusammenhängendes Netz von Stegen, wobei bei genauerer Hinsicht die Topologie
      |nicht flach ist, sondern eine Wand eine andere durchbricht oder überlagert. Es entsteht eine
      |Asymmetrie zwischen gezeichnetem Außen und Innen.
      |""",
    """Ich schaue aus dem Fenster. Der Haltepunkt ist sicher verpasst, doch ich realisiere, dass ich
      |die Macht habe das Gefährt zu stoppen. Dazu versetze ich mich nach außerhalb in eine
      |Entfernung, in der das Gefährt klein wird, so dass ich in die Welt eingreifen und einen
      |Spielstein in den Weg stellen kann. Eine Reihe von Steinen fällt um und bringt das Fahrzeug
      |zum Stehen. Die Bebauung ist spärlich. Mir gegenüber sitzt eine fahl beleuchtete Gestalt, die
      |im Plural ist, nur von `wir' in der Mehrzahl spricht, sich nur als eine von mehreren ausgibt.
      |Zweifel in der Stimme, die sich glücklich behauptet mit den anderen ganz gleich zu sein.
      |""",
    """Ein Screening, bestehend aus zwei Teilen. Existierende Filme wurden verarbeitet. Der zweite
      |Teil mit klassischerer, narrativer, linearer Struktur, der erste experimenteller. Beim
      |Verlassen des Vorführraums spüre ich, dass die Arbeit Naya nicht gefallen hat. Sie sucht nach
      |Formulierungen, je mehr sie sich äußert, desto deutlicher ist, dass es ihr gar nicht gefallen
      |hat. Sicher, viele Dinge sind in kurzer Zeit entstanden, vielleicht es eine weitere
      |Überarbeitung gebraucht, trotzdem war ich zufrieden. Jetzt bin ich enttäuscht und traurig.
      |Verunsichert, dass bei Zuschauern dieser Eindruck nachhaltig zurückbleibt. Jemand sagt, ihr
      |gefielen keine "`Laborfilme"'. Was damit gemeint ist? Es geht offensichtlich um die Ablösung
      |einer Schicht, die dem Film noch anhaftet.
      |""",
    """Zwei Metallkugeln, Coulomb Kugeln, besitzen unterschiedliche Größe von ein oder wenigen
      |Zentimeter Durchmesser. In die kleinere davon ist eine weitere Kugel eingelassen. Etwas wird
      |durch den Kontakt in Gang gesetzt oder durch das wiederholte Zusammenbringen und Trennen. Zwei
      |Stäbe berühren die Kugeln und sind mit Kabeln verbunden, deren anderes Ende zu einer Kiste
      |führt, einem Ton- oder Geräuschgenerator. Man hört den Ton nur durch das Abgreifen eines
      |Signals an der Kiste. Ich bitte die Leute im Raum still zu sein, da ich eine Tonaufnahme mit
      |den Kugeln mache. Die Kugeln lassen sich spielen und produzieren dabei komplexe Klänge, an
      |einer Stelle kann man darin sogar eine Stimme ausmachen.
      |""",
    """In frühen Skizzen erscheinen Knotenpunkte des Raumes als Auge. Oder ist es nur ein
      |Knotenpunkt? Die Pupille ist leer, die Iris wird vom Text gebildet, der sich kreisförmig
      |bewegt. Der Augapfel ist der Schirm mit einer konkaven Linse, in eine Holzkugel eingebettet.
      |Es gibt eine Unschärfezone bei zwölf Uhr, wo Wörter verschwinden und erscheinen, indem sich
      |die Glyphen ineinander verwandeln. Das Graphische der Buchstaben tritt hervor; eine
      |nicht-semantische Überführung. Später konkretisieren sich die Knotenpunkte als Bienenkörbe,
      |der Augapfel verschwindet in ihrem Inneren, nur durch einen Schlitz sichtbar. Das unendliche
      |des Textes ist nun horizontales Auftauchen und Verschwinden, die Kreisförmigkeit nur noch
      |angedeutet durch eine leichte Krümmung des Textlaufes.
      |""",
    """Das sich im Schwarm Bewegen wurde vor Jahrhunderten mit einer wirklichkeitsfremden
      |Begeisterung identifiziert, und Schwarm war das Objekt der Begeisterung. Vielleicht entspringt
      |die Assoziation dem Zerlaufen von Bewegung und Subjekt. Es gibt immer noch Zielrichtungen,
      |doch ihr Magnetismus gehorcht nicht mehr individueller Kontrolle. Wenn wir jedoch immer schon
      |Linienbündel sind, können wir gar nicht anders als schwärmen. Sind wir sich parallel oder
      |sequenziell bewegende Bündel? Man mag denken, dass man nur einen Schritt vor den anderen
      |setzen kann. Doch tatsächlich sind wir als komplexe Zeitmultiplexer in Simultaneitäten
      |verstrickt. Programmieren ist schwärmen; wir können nur Schritt für Schritt den Text
      |entwickeln, befinden uns aber immer in zahlreichen parallelen Programmtexten.
      |""",
    """Theoretisch sind alle neun Mikrocomputer identisch konfiguriert und ausgestattet, einzig durch
      |IP Adresse unterschieden, und doch brauchen alle unterschiedlich lange, bis sie gestartet
      |sind, vielleicht weil sie im Laufe der Softwareentwicklung zu unterschiedlichen Zeitpunkten
      |und mit unterschiedlichen Frequenzen aktualisiert wurden oder die Pakete nicht zeitgleich
      |durch das Netzwerk transportiert wurden. Zunächst war ihr Zeitverhalten noch ähnlicher, und so
      |ergab sich ein Problem der \emph{Redeübernahme}: Mehrere Knoten versuchen gleichzeitig ein
      |Wort zu emittieren, suchen einen Partnerknoten, und durch die Kollision geben sie wieder auf,
      |versuchen es erneut, und immer im Kreis. Die Perioden wurden schließlich randomisiert, das
      |heißt letztendlich, dass die kleinen Differenzen in der Startzeit extrem verstärkt werden.
      |""",
    """Textabschnitte sind auf die Bienenkörbe verteilt, aus der auditiven Ähnlichkeit der Wörter
      |formen sich Bäume. Ich habe mit der graphischen Ansicht dieser Bäume experimentiert, wobei die
      |Wörter durch rot und grün angeben, welchem Korb sie zugehörig sind. Diese Bäume sind in der
      |Ausstellung nicht sicht- sondern nur hörbar. Ein Probedruck auf A3 Papier diente mir fortan
      |als Erklärungshilfe, wann immer jemand genauer wissen wollte, wie die Linien von Wörtern
      |entstehen, die zwischen den Körben wandern. Ich habe das Blatt einem Besucher überlassen, der
      |von dieser Darstellung angetan war. Ich hätte es erneut drucken können, aber es wäre nicht
      |dasselbe Blatt gewesen, das vom vielen Herumtragen bereits abgenutzt war.
      |""",
    """Am Schluß ist wieder alles zerlegt, aufgerollt, auseinandergeschraubt, gepolstert und in
      |Kisten verpackt. Wie geht es weiter? Neue und andere Arbeiten entstehen, die Elemente
      |aufgreifen und fortführen. Das vernetzte Mikrocomputercluster taucht erneut in
      |\emph{wr\_t\_ng m\_ch\_n\_} auf, wo jeder Teil sein eigenes Klanggedächtnis hat. Die Reusen
      |stehen bei uns im Atelier. Nachdem ich begonnen habe, die Frage des algorithmischen
      |\emph{Körpers} zu untersuchen, hängt eine Reuse dort nun an der Wand, wieder mit kleinen
      |Lautsprechern versehen, diesmal mit Porzellan statt Bienenwachs als Schallwand umgeben und mit
      |kleinen Ultraschallsensoren erweitert. Die raumfüllende und raumabhängige Struktur gibt
      |Anleihen zu einer modularen Serie, die noch im Werden begriffen ist.
      |""",
    """Im Begriff des Schwärmens scheint immer eine Ambivalenz zu stecken, im wörtlichen Sinne
      |gegenläufige Wertigkeiten. Beim Ausschwärmen wird neues Territorium gesucht, und andererseits
      |wird das Suchen niemals individuiert, hat also nichts Heroisches an sich (bei Nietzsche wird
      |der Übermensch von Sandkorn-Rücksichten und Ameisen-Kribbelkram bedroht, während sich bei
      |Deleuze und Guattari Löwenmähne und Läuse vereinen). Haben wir tatsächlich den Schwarm
      |gemeint? Michel Serres verbindet den Schwarm in \emph{Genèse} mit dem Vorhandensein einer
      |äußeren Grenze und einer lokalen Individuierung, gegen das er das Multiple setzt, dessen
      |Punkte unbekannt bleiben; das Multiple dringt in den Raum oder entschwindet ihm, sucht sich
      |Platz, gibt ihn auf oder erzeugt ihn durch unvorhersehbare Bewegung.
      |""",
    """Die Computerwelt ist binarisiert; eine Fluchtlinie führt durch zwei Punkte; zwei, vier, acht,
      |sechzehn Kanäle. Wir zählen an zehn Fingern ab. Eine Gruppe benötigt drei Elemente; drei
      |Punkte bilden (meist) eine Ebene; das ödipale Dreieck; Pierces Trichotomien. Nicht genug zum
      |Einfärben einer Landkarte, jedoch angemessen zum Beringen von Leitungen. Eine Null schiebt
      |sich zwischen Eins und Acht. Fünf Ringe für einhundertacht Wege. Eine Unordnung in der
      |Relaismatrix, nachdem sich einige Gewinde der Schraubterminals als abgenutzt erweisen. Drei,
      |fünf, zwölf Volt. Ein Rhythmus von belegten und leeren Terminals, zwei untätige
      |Verstärkerkanäle. Zur binären Teilung braucht man drei Anschlüsse. Zum Laufen drei
      |Möglichkeiten: links, rechts, geradeaus; oder hinauf, hinunter, waagerecht.
      |""",
    """Die Arbeit besteht aus drei verzahnten Klangebenen. Einer wandernden Stimme, deren Worte
      |keiner festen Raumposition zugeordnet sind, einem Pool an summenden und meist von Bienen
      |herrührenden Klängen, wobei jeder Klang einem festen Lautsprecher zugeordnet ist, und dem
      |unaufhörlichen Klicken der Relais, die zwar in räumlicher Distanz zueinander, jedoch in enger
      |Nachbarschaft in der Raummitte liegen. Es gibt eine Klangsituation, der eine Besucherin
      |normalerweise nicht begegnen wird; dann nämlich, wenn die Installation eingeschaltet wird.
      |Nach dem Booten der Computer melden sich alle Relais einmal zu Wort, indem sie in ihren
      |Anfangszustand schalten, und erzeugen für wenige Sekunden eine aufregende und dichte Textur
      |aus Klicken. Ein weiteres Mal beim Ausschalten.
      |""",
    """Die Größenverhältnisse neigen zur Miniaturisierung und erzeugen eine bestimmten Pose des
      |Besuchers. Die Lautsprecher mit 45mm Durchmesser, die Computer mit ihrer Fläche von 85 mal
      |56mm, wenngleich durch Türmung zum Volumen geworden. Der kompakte Relaisklotz. Die durch die
      |Schlitze der Bienenkörbe auf Ausschnitte reduzierten Videobildschirme. Nur ökonomische Gründe?
      |Im Effekt jedenfalls verlangt es ein Stillhalten des Betrachters, ein sich Vorneigen mit Auge
      |und Ohr, eine Vorsichtigkeit. Auch ist es nicht nur die Dimension, die die Miniatur
      |auszeichnet, sondern ebenso (wie Roland Barthes feststellte) eine Präzision und ein
      |rahmenloses Sich-Abheben vom Umgebenden. Ein Spannungsfeld entsteht zum All-Over der
      |Rauminstallation, die gerade die Bewegung organisiert. Das eine braucht das andere.
      |"""
  ).map(mkText)

  val bugWords: Set[String] = Set(
    "gefielen", "Laborfilme", "identifiziert", "befinden", "konfiguriert", "gegenläufige", "flachem", "flach"
  )

  val bugMap: Map[String, String] = bugWords.iterator.map { w =>
    w.replace("fi", "f").replace("fl", "f") -> w.replace("fi", "ﬁ").replace("fl", "ﬂ")
  }.toMap

  // "thanks" to our beloved inkscape
  def debugWords(s: String): String = {
    bugMap.foldLeft(s) { case (in, (k, v)) =>
      var out = in
      while ({
        val i = out.indexOf(k)
        (i >= 0) && {
          out = out.substring(0, i) + v + out.substring(i + k.length)
          true
        }
      }) ()
      out
    }
  }

  /////////////////////////////////////////////////////

  def parseEdges(s: String): Map[(Int, Int), String] = {
    val s1 = s.stripMargin
    val s2 = s1.split("\n\\{").map(_.trim).filter(_.nonEmpty)
    s2.iterator.map { s3 =>
      val i   = s3.indexOf('}')
      val s4  = s3.substring(0, i)
      val s5  = s3.substring(i + 1)
      val j   = s4.indexOf('-')
      val e1  = s4.substring(0, j).toInt
      val e2  = s4.substring(j + 1).toInt
      val s6  = s5.replace("\n", "").trim
      (e1, e2) -> s6
    } .toMap
  }

  val edgesDe: Map[(Int, Int), String] = parseEdges("""
    |{1-7}
    |
    |Ich schaue erneut zurück auf ein starkes Gefälle, wohinunter, zur Missbilligung der Umstehenden, eine Reihe von Steinen fällt.
    |
    |{1-8}
    |
    |Im Vorführraum herrscht eine starke Vertikalität, die sich quer zur Linearität des Filmes vom Boden zur Decke erstreckt.
    |
    |{1-9}
    |
    |Zwei mit hoher Frequenz vibrierende Metallkugeln berühren intermittierend ein riesiges Fenster, das zum Abgreifen eines Signals verwendet wird.
    |
    |{1-10}
    |
    |Ich gehe noch einmal die Knotenpunkte des Raumes entlang. Licht fällt durch leere und zugleich wässerige Pupillen herein.
    |
    |{1-11}
    |
    |In den Gassen schwärmt eine jahrhundertelange Bewegung, gleitet mit wirklichkeitsfremder Begeisterung an Wänden und Seilen hoch, zerläuft schließlich.
    |
    |{1-12}
    |
    |Alle neun Ecken des Raumes sind orangefarben und identisch konfiguriert und ausgestattet, doch kleine Differenzen werden extrem verstärkt.
    |
    |{1-13}
    |
    |Jemand ruft mich an, schüttelt Linien von Wörtern über mich, hat Schwierigkeiten und ein Blatt vor dem Mund.
    |
    |{1-14}
    |
    |Am Schluß sind alle Dinge eng aufgerollt, mit geringsten Amplituden gepolstert und innerhalb von Kisten ohne Griffmöglichkeiten montiert.
    |
    |{1-15}
    |
    |Gibt es beim Ausschwärmen nur einen einzigen Weg oder gegenläufige Abkürzungen? Neues Territorium erscheint als Feld oder Tisch. 
    |
    |{1-16}
    |
    |Alles ist hinunter zu einer Fluchtlinie binarisiert. Ich ziehe mich an zehn Fingern zur binären Teilung nach draußen.
    |
    |{1-17}
    |
    |Die Arbeit besteht aus drei wandernden Stimmen aus Glas, deren Worte meist von der Größe kleiner Äpfel sind.
    |
    |{1-18}
    |
    |Die Größenverhältnisse in der Installation und Ausstellung zu verstehen, verlangt eine bestimmten Pose des Besuchers und seiner Partnerin.
    |""")

//  println(edgesDe.mkString("\n"))
//  println(edgesDe.size)
}
