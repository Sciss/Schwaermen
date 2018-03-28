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
    if (lang == Lang.de)  parDe
    else                  parEn

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

  val parEn: ISeq[String] = List(
    """The alleys are narrow and with a steep slope down to the water. I go back once again.
      |Everything is characterised by a strong verticality. I pull myself up against a wall, on ropes
      |and grips. Is that the only way or just a shortcut? Disapproval of the bystanders? One corner
      |of the room is made of glass, floor to ceiling, a huge window to the outside, orange light
      |enters. Someone calls me, he and his partner have had trouble understanding the installation
      |in the exhibition. There are vibrating things shaking at high frequency and low amplitude,
      |mounted on a field or table the size of small apples.
      |""",
    """It is an experiment. Two human arms are put together, next to each other, close together. They
      |are squeezed or dissected, cut, then united on a microscope slide or glass. A specimen is
      |made, the \emph{section} of the double arm becomes one object. What happens to life? Life was
      |previously separate in each of the arms. To what extent does life unite in the squeezed arms,
      |where it can be traced back to two different lines? Tim Ingold recalls Deleuze's use of the
      |Haecceitas or \emph{bundle} of lines to designate the living organism. The sadness of
      |unrepeatability: united in an experience that never took place except in memory.
      |""",
    """A plan lies in front of me or in my memory. Glass shatters on the floor, but shortly after it
      |can be embedded in a metal again. A small-scale map, I follow the path in my mind or with the
      |finger. Longer than I originally thought, much longer. I see everything close, but from above.
      |It's too far, in maybe an hour it'll be dark, then it will be difficult or even impossible to
      |follow the path. A trap appears. Unrecognizable on the map with its bird's eye view appears a
      |point in which the path drops steeply, it is getting steeper until it goes partly vertically
      |downhill.
      |""",
    """An antique function generator, a thing with a flat screen, approximately in the size 40 by 30
      |centimetres. The screen's background is blue, with a line drawing in white. Functions,
      |geometric functions, lines and circles, plots. They swim around, perhaps moved by interaction,
      |and yield an ensemble of forms. The thought comes to me that we only have finite energy. The
      |shapes emit synthetic sounds that mix together to form a strange overall picture. No
      |surprising sounds these days, but it's easy to imagine how the arrival of the generator made
      |everyone fascinated by the purity of the sounds and their idiosyncratic and slightly chaotic
      |appearance, decay, coalescence.
      |""",
    """A rather dark room in which someone would start to rotate a light source, a red light, and
      |create regular circles that would move around the room (the performer began to move in space).
      |I seemed to remember: yes, everything was red. No. In fact, the colour of the light changed.
      |The works took the visitors around in the room, they had to start running and became
      |performers and the performed in equal measure. Everything accelerated so that people could
      |hardly keep up, but even groups of older people, some with crutches, rushed around as
      |entourage. I did not want to attend the senseless activity and went out.
      |""",
    """We are bent over a drawing, a diagram. In it overlap a cell structure, an organically looped
      |drawing, and a rigid, latticed structure like a grouping of circuits. We are formulating what
      |we like about this structure. On the one hand, calculating functions can be deposited, picked
      |up, inserted on the grid, so that each element of the grid plane appears as the receiver or
      |placeholder of a calculating function. On the other hand, the looped cell plane produced a
      |cross-section, a coherent network of ridges, more precisely, the topology is not flat, but one
      |wall penetrates or overlays another. An asymmetry between drawn outside and inside emerges.
      |""",
    """I look out of the window. The halt was certainly missed, but I realise that I have the power
      |to stop the vehicle. To do this, I shift myself outside to a distance where the vehicle
      |becomes small, so that I can intervene in the world and put a token in the way. A number of
      |stones collapses and brings the vehicle to a halt. The development is sparse. Opposite to me
      |sits a dimly lit figure, who is plural, speaking only of `we' in the plural, posing as one of
      |several. Doubt lies in the voice that claims to be happy as entirely equal with the others.
      |""",
    """A screening, consisting of two parts. Existing films were processed. The second part with more
      |classic, narrative, linear structure, the first more experimental. Upon leaving the screening
      |room, I feel that Naya didn't like the work. She tries to verbalise, the more she speaks, the
      |clearer it becomes that she completely disliked it. Naturally, many things emerged in a short
      |time, maybe needing another revision, yet I was satisfied. Now I'm disappointed and sad.
      |Unsettled that this impression persists with the viewers. Someone says she did not like
      |``laboratory films''. The meaning? It is obviously about the peeling away of a layer that
      |still adheres to the film.
      |""",
    """Two metal balls, Coulomb spheres, with different diameters of one or a few centimetres. In the
      |smaller of them another sphere is embedded. Something is set in motion through contact or
      |through repeated joining and separation. Two bars touch the spheres and are connected to
      |cables whose other end leads to a box, a sound or noise generator. You can only hear the sound
      |by picking up a signal on the box. I ask the people in the room to be quiet, as I make a sound
      |recording with the spheres. They can be played and thereby produce complex sounds, you can
      |even make out a voice once.
      |""",
    """In early sketches, spatial nodes appear as an eye. Or just one node? The pupil is empty, the
      |iris is formed by the circularly moving text. The eyeball is the screen with a concave lens,
      |embedded in a wooden orb. There is a defocusing at twelve o'clock, where words disappear and
      |appear as the glyphs morph into one another. The graphic quality of the letters emerges; a
      |non-semantic transfer. Later, nodes become concrete as beehives, the eyeball disappears in
      |their interior, visible only through a slit. The infinite of the text is now horizontal
      |emergence and disappearance, circularity only hinted at by a slight curvature of the text.
      |""",
    """Centuries ago, moving in the swarm was identified with fanciful enthusiasm, and swarm was the
      |object of exaltation. Perhaps the association springs from the melting of movement and
      |subject. There are still directions, but their magnetism no longer obeys individual control.
      |But if we are always already bundles of lines, we cannot help but swarm. Are we parallel or
      |sequentially moving bundles? One may think that one can only step one step ahead of the other.
      |But actually, as complex time multiplexers, we are entangled in simultaneities. Programming is
      |swarming; we can only develop the text step by step, but we are always inside numerous
      |parallel source codes.""",
    """Theoretically, all nine microcomputers are identically configured and equipped, distinguished
      |only by IP address, and yet all take different durations to boot, perhaps because in the
      |course of software development they were updated at different times and at different
      |frequencies, or packets were not synchronously delivered across the network. Initially, their
      |temporal behaviour was more similar, and so there was a problem of \emph{turn-taking}: several
      |nodes simultaneously trying to emit a word, looking for a partner node, and giving up due to
      |collisions, trying again, and always in a loop. The periods were eventually randomised, which
      |essentially means that the small differences in boot time are extremely amplified.
      |""",
    """Text sections are distributed across the beehives, trees are formed from the auditory
      |similarity of the words. I experimented with the graphical view of these trees, red and green
      |words indicating which basket they belong to. In the exhibition, these trees are audible but
      |not visible. A test print on A3 paper henceforth served as a figure of explanation whenever
      |someone wanted to know more precisely how the lines of words moving between the baskets are
      |formed. I left the sheet to a visitor who fell for this rendering. I could have reprinted it,
      |but it would not have been the same leaf, worn off from carrying around.
      |""",
    """Finally, everything is disassembled, rolled up, unscrewed, padded and packed in boxes. What's
      |next? New and different works emerge that seize and evolve elements. The cluster of networked
      |microcomputers reappears in \emph{wr\_t\_ng m\_ch\_n\_}, where each part has its own sound
      |memory. The traps are in our studio. There, after I started to investigate the question of the
      |algorithmic \emph{body}, a trap hangs on the wall, again furnished with small loudspeakers,
      |this time surrounded with porcelain instead of beeswax as a baffle, and extended by tiny
      |ultrasound sensors. The space-filling and space-dependent structure gives borrowings to a
      |modular series that is still in the making.""",
    """The concept of swarming always seems to contain an ambivalence, literally as opposing
      |valences. Swarming seeks new territory, but then again, the search is never individuated, thus
      |it partakes of nothing heroic (Nietzsche's overman is threatened by sand-grain sized
      |considerations and the detritus of swarming ants, while Deleuze and Guattari unite lion's mane
      |and lice). Did we really mean swarm? In \emph{Genèse}, Michel Serres links the swarm with the
      |presence of an outer boundary and a local individuation, against which he places the multiple
      |whose points remain unknown; the multiple invades the space or disappears from it, seeks
      |place, gives it up or creates it by unpredictable movement.
      |""",
    """Computers are binarised; a line of flight leads through two points; two, four, eight, sixteen
      |channels. We count on ten fingers. A group requires three elements; three points (usually)
      |form a plane; the oedipal triangle; Pierce's trichotomies. Not enough to colourise a map, but
      |appropriate to ring wires. A zero moves between one and eight. Five rings for 108 ways. The
      |relay matrix cluttered, after some threads of the screw terminals prove to be worn out. Three,
      |five, twelve volts. A rhythm of occupied and idle terminals, two inactive amplifier channels.
      |Binary division requires three connections. Running requires three choices: left, right,
      |straight ahead; or up, down, horizontally.
      |""",
    """The work consists of three interlocked sound layers. A wandering voice whose words are not
      |associated with a fixed spatial position, a pool of buzzing sounds mostly originating from
      |bees---each sound being associated with a fixed loudspeaker---and the relentless clicking of
      |relays, spaced apart but in close proximity in the middle of the room. There is a sound
      |situation that a visitor will normally not encounter; namely, when the installation is turned
      |on. After booting the computers, all the relays will speak once by switching to their initial
      |state, creating an exciting and dense texture of clicks for a few seconds. And again when
      |shutting down.
      |""",
    """The proportions tend to miniaturisation and generate a certain posture of the visitor.
      |Loudspeakers at 45mm diameter, computers having a surface of 85 times 56mm, though becoming a
      |volume by piling. The compact relay block. Video screens, reduced to cutouts by the slots of
      |the beehives. Economic reasons? Anyhow, they effectively require standstill of the viewer,
      |leaning forward with eye and ear, cautiousness. Not only the dimension distinguishes the
      |miniature, but also (as Roland Barthes noted) a precision and a frameless being silhouetted
      |against the surrounding. A field of tension is created toward the all-over of the installation
      |that is precisely organising the movement. One needs the other.
      |""".stripMargin
  ).map(mkText)

  val numPar: Int = {
    val res1 = parDe.size
    val res2 = parEn.size
    assert(res1 == 18)
    assert(res2 == res1)
    res1
  }

  val bugWords: Set[String] = Set(
    "befinden", "flach", /* "flache", "flachem", */ "gefielen", "gegenläufige", "identifizier",
    /* "identifizieren", "identifiziert", */ "konfiguriert", "Laborfilme",
    "amplifie", "baffle", "configured", "difficult", "field", "figure", "filling", "film", "finger", "finite",
    "first", "five", "fixed", "flat", "flight", "floor", "identified", "satisfied"
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
    if (lang == Lang.de)  edgesDe
    else                  edgesEn

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

  val edgesEn: Map[(Int, Int), String] = parseEdges("""
    |{1-7}
    |
    |Once again I look back on a steep incline, where below, to the disapproval of bystanders, stones collapse.
    |
    |{1-8}
    |
    |In the projection room, there's a strong verticality, extending across the linearity of the film, floor to ceiling.
    |
    |{1-9}
    |
    |Two metal balls, vibrating at high frequency vibrating, intermittently touch a huge window used to register a signal.
    |
    |{1-10}
    |
    |Again, I walk along the nodes of the room. Light enters through empty and at once watery pupils.
    |
    |{1-11}
    |
    |In the alleyways, a century-long movement swarms, glides up walls and ropes with fanciful enthusiasm, and finally disintegrates.
    |
    |{1-12}
    |
    |All nine corners of the room are orange, identically configured and equipped, but small differences are extremely amplified.
    |
    |{1-13}
    |
    |Someone is calling me, shaking lines of words across me, having difficulties and a leaf outside their mouth.
    |
    |{1-14}
    |
    |At last, all things are tightly rolled up, padded with minimal amplitudes and mounted inside boxes without grips.
    |
    |{1-15}
    |
    |Is there only one way or opposite shortcuts when swarming? New territory appears as a field or table.
    |
    |{1-16}
    |
    |Everything is binarised down to lines of flight. As binary division, I pull myself outside on ten fingers.
    |
    |{1-17}
    |
    |The work consists of three wandering voices made of glass, the words usually the size of small apples.
    |
    |{1-18}
    |
    |To understand proportions in the installation and exhibition requires a certain posture of the visitor and his partner.
    |
    |{2-7}
    |
    |Life was formerly separate in me and the vehicle that is stopped by a tile at the halt.
    |
    |{2-8}
    |
    |Two human arms were briefly processed by squeezing a layer, cutting it and then apparently peeling it off.
    |
    |{3-9}
    |
    |A trap of few centimetres in diameter emerges, set in motion by a smaller map lying before me.
    |
    |{4-10}
    |
    |Sketches show a function generator, something with an eyeball, a curious totality of glyphs morphing into each other.
    |
    |{5-11}
    |
    |The works identify themselves as objects of enthusiasm, so it seemed, because their magnetism obeyed a senseless activity.
    |
    |{6-12}
    |
    |Throughout the software development, we are bent over groupings of different frequencies, appearing as randomised periods or placeholders.
    |"""
  )
}
