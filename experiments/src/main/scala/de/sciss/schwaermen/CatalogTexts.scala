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

import de.sciss.schwaermen.Catalog.Lang

import scala.collection.immutable.{IndexedSeq => Vec, Seq => ISeq}

object CatalogTexts {

  private def mkText(in: String): String = {
    val s0 = in.stripMargin
    val s1 = s0.replace('\n', ' ')
    val s2 = s1.replace("  ", " ")
    s2.trim
  }

  def par(lang: Lang): ISeq[String] =
    if (lang == Lang.de) parDe
    else ???

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
      |auf zwei unterschiedliche Linien zurückverfolgt werden kann? Tim Ingold erinnert an Deleuzes
      |Verwendung der Haecceitas oder des \emph{Bündels} von Linien zur Bezeichnung des lebendigen
      |Organismus. Die Traurigkeit der Unwiederholbarkeit: In einer Erfahrung vereint, die jedoch nie
      |stattfand, außer in der Erinnerung.
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
      |hat. Sicher, viele Dinge sind in kurzer Zeit entstanden, vielleicht hätte es weitere
      |Überarbeitungen gebraucht, trotzdem war ich zufrieden. Jetzt bin ich enttäuscht und traurig.
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
    """Am Schluss ist wieder alles zerlegt, aufgerollt, auseinandergeschraubt, gepolstert und in
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

  val numPar: Int = {
    val res = parDe.size
    assert(res == 18)
    res
  }

  val bugWords: Set[String] = Set(
    "befinden", "flach", /* "flache", "flachem", */ "gefielen", "gegenläufige", "identifizier",
    /* "identifizieren", "identifiziert", */ "konfiguriert", "Laborfilme"
  )

  val bugMap: Map[String, String] = bugWords.iterator.map { w =>
    w.replace("fi", "@").replace("fl", "f").replace("@", "f") -> w.replace("fi", "ﬁ").replace("fl", "ﬂ")
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

  /** Guarantees that `res._1 < res._2`. */
  def parIdToIndex(id1: Int, id2: Int): (Int, Int) = {
    require(id1 != id2)
    val idx1 = parIdToIndex(id1)
    val idx2 = parIdToIndex(id2)
    if (idx1 < idx2) (idx1, idx2) else (idx2, idx1)
  }

  def parIdToIndex(id: Int): Int = {
    val res = parOrder.indexOf(id)
    require(res >= 0)
    res
  }

  def parIdxToId(idx: Int): Int = parOrder(idx)

  /** Guarantees that `res._1 < res._2`. */
  def parIdxToId(idx1: Int, idx2: Int): (Int, Int) = {
    require(idx1 != idx2)
    val id1 = parIdxToId(idx1)
    val id2 = parIdxToId(idx2)
    if (id1 < id2) (id1, id2) else (id2, id1)
  }

  val parOrder: Vec[Int] = Vector(
    13, 14, 15,
     7,  8,  9,
     1,  2,  3,
     4,  5,  6,
    10, 11, 12,
    16, 17, 18
  )

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

  /** Maps from (srcParId, tgtParId) to text.
    *
    * __Note:__ these are 1-based identifiers, not indices.
    */
  def edges(lang: Lang): Map[(Int, Int), String] =
    if (lang == Lang.de) edgesDe
    else ???

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
    |Am Schluss sind alle Dinge eng aufgerollt, mit geringsten Amplituden gepolstert und innerhalb von Kisten ohne Griffmöglichkeiten montiert.
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
    |
    |{2-7}
    |
    |Das Leben war vormalig separat in mir und dem Gefährt, das ein Spielstein am Haltepunkt zum Stehen bringt.
    |
    |{2-8}
    |
    |Zwei menschliche Arme wurden in kurzer Zeit verarbeitet, indem eine Schicht gequetscht, geschnitten und dann offensichtlich abgelöst wird.
    |
    |{2-9}
    |
    |Man hört den Ton in jedem der Arme unterschiedlicher Größe nur, wenn sie wiederholt zusammengebracht und getrennt werden.
    |
    |{2-10}
    |
    |Mit einer konkaven Linse werden zwei unterschiedliche Linien kreisförmig bewegt und in einem Knotenpunkt zu Deleuzes Organismus zurückverfolgt.
    |
    |{2-11}
    |
    |Entspringt unser Subjekt sich parallel oder sequenziell bewegenden, zusammengelegten, zusammengequetschten Armen, deren Magnetismus nicht mehr individueller Kontrolle gehorcht?
    |
    |{2-12}
    |
    |Alle Experimente werden zusammengelegt, nebeneinander, dicht beieinander, und brauchen letztendlich doch alle unterschiedlich lange, bis sie seziert sind.
    |
    |{2-13}
    |
    |Inwieweit vereint sich die auditive Ähnlichkeit der Wörter, die vom vielen Herumtragen einem Bündels überlassen wurden, zur Haecceitas? 
    |
    |{2-14}
    |
    |Die Traurigkeit der Unwiederholbarkeit wird zerlegt, auseinandergeschraubt, und unter Verwendung von Porzellan statt Bienenwachs als Exemplar wieder hergestellt.
    |
    |{2-15}
    |
    |Im doppelten Arm scheint eine Ambivalenz zu stecken, welche dann als Guattaris Löwenmähne vereint auf einem Objektträger bleibt.
    |
    |{2-16}
    |
    |Zur Bezeichnung von Linien (Fluchtlinien) führen zwei, vier, acht, jedoch nie einhundertacht Punkte zu einem Ausschnitt des Lebens.
    |
    |{2-17}
    |
    |Unaufhörliche, verzahnte Erinnerungen erinnern an Klicken von Glas, das für wenige Sekunden in enger Nachbarschaft zur Raummitte stattfand.
    |
    |{2-18}
    |
    |Bei Miniaturisierung handelt es sich um eine lebendige Erfahrung dessen, was beim Organisieren der Schlitze der Bienenkörbe passiert.
    |
    |{3-7}
    |
    |Die Bebauung ist in Metall eingefasst, das sich in den Weg stellt. In meinem Gedächtnis zerspringt ein Glas.
    |
    |{3-8}
    |
    |Existierende Karten haben einen kleinen Maßstab, der Zuschauer von "`Laborfilmen"' traurig macht und verunsichert. Was damit gemeint ist?
    |
    |{3-9}
    |
    |Eine Falle von wenigen Zentimeter Durchmesser taucht auf, in Gang gesetzt durch einen kleineren, vor mir liegenden Plan.
    |
    |{3-10}
    |
    |Die Iris wird vom Text gebildet, der in einer Unschärfezone ganz nah, von oben in der Vogelperspektive erscheint.
    |
    |{3-11}
    |
    |Der steil abfallende Weg entspringt der Assoziation von Bewegung und Subjekt, verfolgt mich im Geiste mit dem Finger.
    |
    |{3-12}
    |
    |Theoretisch sind Mikrocomputer schwierig bis unmöglich auf der Karte zu verfolgen, in ihrem Zeitverhalten unmerkbar und teilweise senkrecht.
    |
    |{3-13}
    |
    |Länger und immer steiler, als ich ursprünglich dachte, habe ich mit der bereits abgenutzten Ansicht dieser Textabschnitte experimentiert.
    |
    |{3-14}
    |
    |Wenig später geht es wieder weiter, andere Arbeiten entstehen, wo jede Reuse in ihrem eigenen Klanggedächtnis bergab geht.
    |
    |{3-15}
    |
    |Eine Stelle, an der im wörtlichen Sinne lokale Individuierung doch wieder gegen das Multiple von Sandkorn-Rücksichten gesetzt wird.
    |
    |{3-16}
    |
    |Es ist zu weit, die Computerwelt führt durch eine Gruppe von drei Elementen, benötigt einen viel längeren Weg.
    |
    |{3-17}
    |
    |In vielleicht einer Stunde meldet sich ein Pool an summenden Bienen einmal zu Wort, keinen festen Raumpositionen zugeordnet.
    |
    |{3-18}
    |
    |Ich sehe den Weg, wenngleich durch Türmung zum Volumen und dunkel geworden, lässt er sich eine Präzision anmerken.
    |
    |{4-7}
    |
    |Ich realisiere, dass der Hintergrund des flachen, fahl beleuchteten Schirms zwar die Macht jedoch nur endliche Energie hat.
    |
    |{4-8}
    |
    |Jemand sagt, ihr gefielen die blau-weißen geometrischen Strichzeichnungen nicht, von denen der nachhaltige Eindruck eines umherschwimmenden Ensembles zurückblieb.
    |
    |{4-9}
    |
    |Durch Interaktion zweier Coulomb Kugeln aus der Frühzeit verband sich die eigenwillige Reinheit der Klänge mit  chaotischen Kreisen.
    |
    |{4-10}
    |
    |In Skizzen erscheint ein Funktionsgenerator, ein Ding mit Augapfel und einem merkwürdigen Gesamtbild von sich ineinander verwandelnden Glyphen.
    |
    |{4-11}
    |
    |Als komplexe Zeitmultiplexer mischen sich Funktionen, Linien und Plots zusammen, bewegen und ergeben Schritt für Schritt synthetische Formen.
    |
    |{4-12}
    |
    |Zu unterschiedlichen Zeitpunkten kommen mir Gedanken zeitgleicher Worte, die bei ihrer Ankunft schließlich im Kreis zu kollidieren suchen.
    |
    |{4-13}
    |
    |Wann immer sich geometrische Bäume formen, kann man sich leicht vorstellen, dass sie rot und grün bewegt werden.
    |
    |{4-14}
    |
    |Erneut erscheinen Schallwände mit kleinen Reusen versehen, haben begonnen modulare Serien aufzugreifen, die noch im Werden begriffen sind.
    |
    |{4-15}
    |
    |Ameisen-Kribbelkram entschwindet in den Raum, dringt in Bildschirme ein, ergibt überraschende Klänge der Größe 40 mal 30 Zentimeter.
    |
    |{4-16}
    |
    |Sechzehn geometrische Funktionen zu drei Dreiecken, ödipale Klänge in untätigen Verstärkerkanälen, nicht genug zum Einfärben der abgenutzten Schraubterminals.
    |
    |{4-17}
    |
    |Feste Klangebenen gehen unaufhörlich vorüber und gehen unaufhörlich zusammen, rühren mit eingeschalteten Generatoren in räumlicher Distanz zueinander herum.
    |
    |{4-18}
    |
    |Alles Stillhalten war fasziniert von der Miniaturisierung geometrischer Klänge und von deren rahmenlosen Sich-Abheben von umgebenden ökonomischen Funktionen.
    |
    |{5-7}
    |
    |Ich versetze mich außerhalb einer Lichtquelle, werde eine dunkle Gestalt im Plural. Mein `wir' hetzt als Gefolge umher.
    |
    |{5-8}
    |
    |Der Performer sucht nach Formulierungen zur Farbe des Lichtes, das zu rotieren und regelmäßige narrative Kreise erzeugen würde.
    |
    |{5-9}
    |
    |Kugeln und Kabel bewegen sich still im Raum, beschleunigen sich etwas, werden eher zu Performern denn zum Performten.
    |
    |{5-10}
    |
    |Als Auge oder Holzkugel nahmen die Besucher ein rotes Licht mit herum im Raum, konnten später kaum mithalten.
    |
    |{5-11}
    |
    |Die Arbeiten identifizieren sich als Objekte der Begeisterung, so schien mir, denn ihr Magnetismus gehorchte einem sinnlosen Treiben.
    |
    |{5-12}
    |
    |Als selbst Gruppen älterer Menschen versuchten gleichzeitig durch das Netzwerk transportiert zu werden, übernahm ich wieder die Rede.
    |
    |{5-13}
    |
    |Ein Besucher musste zu rennen anfangen, da er genauer wissen wollte, wie sich Wörter durch den Raum verteilen.
    |
    |{5-14}
    |
    |Das vernetzte Cluster aus Mikrocomputern war rot verpackt, erinnerte sich jemand. Es hängt nun dort an der Wand.
    |
    |{5-15}
    |
    |Nietzsche wollte dem Übermensch nicht beiwohnen und ging auf Krücken hinaus, suchte sich Platz, machte eine unvorhergesehene Bewegung.
    |
    |{5-16}
    |
    |Eine Null schiebt sich zwischen Pierces Trichotomien und beginnt sich im Raum zu bewegen. Wir zählen die Kanäle.
    |
    |{5-17}
    |
    |Nein, in der Tat änderte sich der Anfangszustand der Relais, nämlich dann, wenn die Leute alle Relais schalteten.
    |
    |{5-18}
    |
    |Ein eher kompakter Relaisklotz, welcher sich im Raum auf Ausschnitte reduziert, alles gleichermaßen als Präzision und Spannungsfeld organisiert.
    |
    |{6-7}
    |
    |Die Stimme auf dem Gitterblatt ist zu spärlich, um in die Welt einzugreifen, überlappt sich mit ihrer Realisation.
    |
    |{6-8}
    |
    |Beim Verlassen des Vorführraums zeichnet sich ein rigides, gitterförmiges Gefüge ab, ist bei genauerer Hinsicht eine rechnende Topologie.
    |
    |{6-9}
    |
    |Am anderen Ende der Tonaufnahme überlagert ein zusammenhängendes Netz von Stegen und Kugeln eine flache, organisch geschlungene Zellebene.
    |
    |{6-10}
    |
    |Bei zwölf Uhr verschwindet diese Struktur in einem Querschnitt, wo sich eine Wand konkretisiert und eine andere durchbricht.
    |
    |{6-11}
    |
    |Wenn Schaltkreise jedoch immer schon Linienbündel sind, Zellstrukturen und Gitterebenen, dann man mag denken, dass individuelle Verstrickungen einsetzen.
    |
    |{6-12}
    |
    |Im Laufe der Softwareentwicklung stehen wir über Gruppierungen unterschiedlicher Frequenzen gebeugt, die als randomisierte Perioden oder Platzhalter erscheinen.
    |
    |{6-13}
    |
    |Ich hätte die rechnenden Funktionen erneut als geschlungene Zeichnung drucken können, aber sie wären fortan nicht sichtbar gewesen.
    |
    |{6-14}
    |
    |Zwischen Außen und Innen entsteht ein algorithmischer Körper, produziert sich als raumabhängige Struktur, umgeben von Händen und Empfängern.
    |
    |{6-15}
    |
    |Am Begriff des Schwärmens gefällt jedes Element als niemals individuiert, hat also nichts Abgelagertes, das äußeren Grenzen verbindet.
    |
    |{6-16}
    |
    |Zum einen lässt sich eine Unordnung in der Relaismatrix formulieren, zum anderen braucht man fünf Ringe zum Laufen.
    |
    |{6-17}
    |
    |Besucherinnen begegnen normalerweise Graphiken oder Diagrammen nicht, so dass sich eine Asymmetrie aus aufregendem und dichtem Klicken ergibt.
    |
    |{6-18}
    |
    |Wir sind dabei zu ergründen, warum nicht Funktionen sondern Vorsichtigkeiten im Stillhalten der Betrachter die Miniatur auszuzeichnen scheinen.
    |
    |{7-13}
    |
    |Zwischen den Körben wandern Gefährte und Gefährten, die ich trotz Zugehörigkeit mir gegenüber sicher in einiger Entfernung verpasse.
    |
    |{7-14}
    |
    |Aus dem Fenster führen neue Elemente ins Atelier fort. In der Mehrzahl spricht man von Anleihen an Maschinen.
    |
    |{7-15}
    |
    |Dies zu stoppen, behauptet immer das Heroische für sich, ganz gleich welche Zweifel das Vorhandensein der Punkte erzeugt.
    |
    |{7-16}
    |
    |Ich kann mich glücklich messen, eines von mehreren Fahrzeugen zum Beringen auszugeben. Einige Gewinde erweisen sich als waagerecht.
    |
    |{7-17}
    |
    |Dazu wird jeder Klang einem Lautsprecher zugeordnet, so dass ein weiteres Mal beim Ausschalten nur Texturen erzeugt werden.
    |
    |{7-18}
    |
    |Im Effekt jedenfalls türmen sich kleine Volumen von 45 mal 85 mal 56mm in den Schlitze der Bienenkörbe.
    |
    |{8-13}
    |
    |Diese Bäume sind in der Ausstellung nicht Erklärungshilfe, sondern eine klassische, narrative Struktur, der die Überarbeitung noch anhaftet.
    |
    |{8-14}
    |
    |Fragen des Screening bestehen aus zwei Teilen, wobei sich der zweite Teil im Neuen und im Schreiben äußert.
    |
    |{8-15}
    |
    |Die Läuse vereinen sich zur Zufriedenheit von Michel Serres, andererseits wurden sie von unbekannten Schwärmen aufgesucht und verarbeitet.
    |
    |{8-16}
    |
    |Es ist deutlicher zu spüren, dass ihr die Arbeit des Einfärbens nicht gefallen hat und sie enttäuscht hat.
    |
    |{8-17}
    |
    |Vielleicht hätte es ein weiteres Booten der Computer gebraucht, das dem Film eine desto experimentellere Klangsituation auferlegt hätte.
    |
    |{8-18}
    |
    |Sicher, viele Dinge sind gerade durch Vorneigen mit Auge und Ohr entstanden, trotzdem waren sie ohne weiteres flächig.
    |
    |{9-13}
    |
    |Im Probedruck auf Papier produziert der Geräuschgenerator eine weitere, nicht nur hörbare Darstellung, die sogar eine Stimme besitzt.
    |
    |{9-14}
    |
    |Dabei führen komplexe Klänge aus Ultraschalllautsprechern an einer Stelle zu einem Kontakt mit Kugeln, die den Raum ausfüllen.
    |
    |{9-15}
    |
    |Eine Kiste führt an Kisten vorbei, wird gesucht und vom Multiplen bedroht, wird eingelassen und lässt sich ein.
    |
    |{9-16}
    |
    |Ich bitte die Kugel, sich links und rechts und spielend zu leeren. Zwölf Leute machen fünf Terminals aus.
    |
    |{9-17}
    |
    |Wobei man mit einem Ton oder mit einem Stab jedoch darin, davon, oder dadurch sein Sein ausmachen kann.
    |
    |{9-18}
    |
    |Durch zwei Dimensionen, wie Roland Barthe feststellen wird, werde ich die Bewegung ebenso gerade wie mich selbst machen.
    |
    |{10-13}
    |
    |Es gibt das Graphische der Buchstaben auch auf den Bienenkörben, in ihrer Kreisförmigkeit eingebettet, nur noch leicht gekrümmt.
    |
    |{10-14}
    |
    |Nicht-semantische Sensoren sind für uns nur durch einen kleinen Schlitz in unserem Augapfel sichtbar beziehungsweise als Andeutung verpackt.
    |
    |{10-15}
    |
    |Das unendliche des Textes wird in seiner Genese nun immer und nur durch eine Skizze in Wertigkeiten überführt.
    |
    |{10-16}
    |
    |Nicht genug erscheinen in den frühen drei Ebenen horizontale Wörter als Leitungen, die sich meist in Dreiecken brechen.
    |
    |{10-17}
    |
    |In ihrem Inneren verschwinden Klänge als Bienen, tauchen hin und wieder als Körbe auf und als Textläufe hervor.
    |
    |{10-18}
    |
    |Wo der Schirm verschwindet, treten die Lautsprecher zum hervor, wodurch das eine dazu neigt das andere zu brauchen.
    |
    |{11-13}
    |
    |In welchem Korb dasselbe Blatt gewesen ist, darüber gibt es immer zahlreiche Tatsachenvermutungen, wir können gar nicht anders.
    |
    |{11-14}
    |
    |Wenn man nur einen Schritt untersucht, taucht ein Teil davon vielleicht wieder als paralleles oder sequenzielles Bündel auf.
    |
    |{11-15}
    |
    |Während sich bei Deleuze das Schwärmen in parallelen Programmtexten entwickelt, haben wir tatsächlich keine Kontrolle über ihre Zielrichtungen.
    |
    |{11-16}
    |
    |Ein Rhythmus kann sich vor den anderen setzen, acht Anschlüsse belegen, doch befinden wir uns immer im Text.
    |
    |{11-17}
    |
    |Zwar gibt es meist nur ein Programmieren im Schwarm, doch das können wir nur in der Installation sein.
    |
    |{11-18}
    |
    |Es schwärmen die Simultaneitäten, wir sind vor uns nicht mehr sicher und nur zu einem All-Over Computer geworden.
    |
    |{12-13}
    |
    |Drei graphische Bäume dienten mir als geknotete Partner, wobei ich einzig in der Startzeit vom Entstehen angetan war.
    |
    |{12-14}
    |
    |Nachdem ich diesmal bei mehreren Knoten stand und sie erneut zu erweitern suchte, startete die Aktualisierung ähnlicher Pakete.
    |
    |{12-15}
    |
    |Was wird gemeint sein durch die Aufgabe der IP Adresse? Wird der Schwarm durch sie vielleicht versuchen einzudringen?
    |
    |{12-16}
    |
    |Drei Punkte bilden drei Möglichkeiten: Geradeaus, hinauf, hinunter, jedoch nur ein Weg auf der Landkarte wird so unterschieden.
    |
    |{12-17}
    |
    |Indem sie in einem fort am Emittieren ist, ergab sich zunächst ein Problem, das es zu immergrünen heißt.
    |
    |{12-18}
    |
    |Wenngleich durch die Bildschirme nicht allein Durchmesser sondern Rauminstallationen entstehen, sind sie noch als Erzeuger von Videos entstanden.
    |""")

//  println(edgesDe.mkString("\n"))
//  println(edgesDe.size)
}
