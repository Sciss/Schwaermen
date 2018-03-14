/*
 *  Catalog.scala
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

import java.awt.Font
import java.awt.geom.AffineTransform
import java.io.FileOutputStream
import java.nio.channels.Channels

import de.sciss.file._
import de.sciss.numbers

import scala.collection.immutable.{Seq => ISeq}

object Catalog {
  def main(args: Array[String]): Unit = {
//    val t0 = Transform(1.2, 3.4, 5.6, 7.8, 9.10, 11.12)
//    val t1 = Transform.fromAwt(t0.toAwt)
//    assert(t0 == t1)

    run()
  }

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
    "gefielen", "Laborfilme", "identifiziert", "befinden", "konfiguriert", "gegenläufige"
  )

  val bugMap: Map[String, String] = bugWords.iterator.map(w => w.replace("fi", "f") -> w.replace("fi", "ﬁ")).toMap

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

  val latexTemplate: String =
    """@documentclass[10pt,twoside,twocolumn]{article}
      |@usepackage[paperheight=20cm,paperwidth=20cm,top=25mm,bottom=25mm,right=25mm,left=25mm,heightrounded]{geometry}
      |@usepackage[ngerman]{babel}
      |@usepackage{Alegreya}
      |@usepackage[T1]{fontenc}
      |@usepackage[utf8]{inputenc}
      |
      |@begin{document}
      |@pagestyle{empty}
      |@fontsize{8.5pt}{10.2pt}\selectfont
      |@noindent
      |%s
      |@end{document}""".stripMargin.replace('@', '\\')

  def writeText(s: String, f: File): Unit = {
    val fos = new FileOutputStream(f)
    try {
      fos.write(s.getBytes("UTF-8"))
    } finally {
      fos.close()
    }
  }

  // these you need to have installed
  val pdflatex: String = "pdflatex"
  val inkscape: String = "inkscape"
  val pdftk   : String = "pdftk"

  case class Style(font: Font)

  object Point2D {
    def fromAwt(in: java.awt.geom.Point2D): Point2D =
      apply(in.getX, in.getY)
  }
  case class Point2D(x: Double, y: Double) {
    def toAwt: java.awt.geom.Point2D = new java.awt.geom.Point2D.Double(x, y)
  }

  object Transform {
    def fromAwt(in: AffineTransform): Transform =
      apply(in.getScaleX, in.getShearY, in.getShearX, in.getScaleY, in.getTranslateX, in.getTranslateY)
  }
  case class Transform(a: Double, b: Double, c: Double, d: Double, e: Double, f: Double) {
    private[this] lazy val at = new AffineTransform(a, b, c, d, e, f)

    def toAwt: AffineTransform = new AffineTransform(at)

    def apply(pt: Point2D): Point2D =
      Point2D.fromAwt(at.transform(pt.toAwt, null))
  }

  case class TSpan(id: String, y: Double, x: ISeq[Double], text: String, node: xml.Elem) {
    def start: Point2D = Point2D(x.head, y)

    def size: Int = x.size

    def apply(idx: Int): TSpan = slice(idx, idx + 1)

    def slice(from: Int, until: Int): TSpan = {
      val id1 = s"$id-$from-$until"
      val x1  = x.slice(from, until)
      val t1  = text.substring(from, until)
      val n1  = setId(node, id1)
      val n2  = setAttr(n1, "x", x1.map(_.toFloat).mkString(" "))
      val n3  = n2.copy(child = new xml.Text(t1) :: Nil)
      copy(id = id1, y = y, x = x1, text = t1, node = n3)
    }
  }

  def setAttr(in: xml.Elem, key: String, value: String): xml.Elem = {
    val a = xml.Attribute(pre = null, key = key, value = value, next = xml.Null)
    in % a
  }

  def setId(in: xml.Elem, id: String): xml.Elem =
    setAttr(in, key = "id", value = id)

  case class Text(id: String, style: Style, transform: Transform, children: ISeq[TSpan], node: xml.Elem) {
    def setTransform(t: Transform): Text = {
      val ts = s"matrix(${t.a.toFloat},${t.b.toFloat},${t.c.toFloat},${t.d.toFloat},${t.e.toFloat},${t.f.toFloat})"
      val n1 = setAttr(node, "transform", ts)
      copy(node = n1)
    }

    def splitAt(idx: Int): (Text, Text) = {
      val (c1, c2) = node.child.splitAt(idx)
      val (n1, n2) = (node.copy(child = c1), node.copy(child = c2))
      val (c3, c4) = children.splitAt(idx)
      (copy(children = c3, node = n1), copy(children = c4, node = n2))
    }

    def size: Int = children.size

    def apply(idx: Int): Text = slice(idx, idx + 1)

    def slice(from: Int, until: Int): Text = {
      val id1 = s"$id-$from-$until"
      val c1  = node.child.slice(from, until)
      val n1  = node.copy(child = c1)
      val n2  = setId(n1, id1)
      val c3  = children.slice(from, until)
      copy(id = id1, children = c3, node = n2)
    }

    def chop(idx: Int): ISeq[Text] = {
      val t1  = apply(idx)
      val ts  = t1.children.head
      ISeq.tabulate(ts.size) { i =>
        val id2 = s"${t1.id}-ch$i"
        val ts1 = ts(i)
        val n1  = node.copy(child = ts1.node)
        val n2  = setId(n1, id2)
        t1.copy(id = id2, children = ts1 :: Nil, node = n2)
      }
    }
  }

  def parseStyle(n: xml.NodeSeq): Style = {
    val m: Map[String, String] = n.text.split(';').iterator.flatMap(_.split(':') match {
      case Array(key, value) => Some((key, value))
      case _ => None
    }) .toMap

//    (key, value) match {
//      case "font-style:italic;font-variant:normal;font-size:8.46819973px;font-family:Alegreya;"
//    }

    val fntSize   = m("font-size")
    val fntFamily = m("font-family")
    val fntSizeD  = {
      // XXX TODO -- somehow inkscape treats 1 px = 1 pt here?
      require (fntSize.endsWith("px"))
      fntSize.substring(0, fntSize.length - 2).toFloat
    }
    val name      = if (m.get("font-style").contains("italic")) s"$fntFamily Italic" else fntFamily
    val font      = new Font(name, Font.PLAIN, 1).deriveFont(fntSizeD)

    Style(font) // fontOpt.map(Style).getOrElse(sys.error(s"Could not parse style from $n and $m"))
  }

  def parseTransform(n: xml.NodeSeq): Transform = {
    val t = n.text.trim
    if (t.startsWith("matrix(") && t.endsWith(")")) {
      val in = t.substring(t.indexOf('(') + 1, t.lastIndexOf(')'))
      in.split(',').map(_.toDouble) match {
        case Array(a, b, c, d, e, f) => Transform(a, b, c, d, e, f)
        case _ => sys.error("no")
      }

    } else sys.error(s"Could not parse transform $n")
  }

  def parseTSpan(n: xml.Node): TSpan = n match {
    case e0: xml.Elem if e0.label == "tspan" =>
      val id = (n \ "@id").text
      val y = (n \ "@y").text.trim.toDouble
      val x = (n \ "@x").text.split(' ').iterator.map(_.trim.toDouble).toList
      val text0 = n.text.trim
      val text  = debugWords(text0)
      val e = if (text == text0) e0 else {
        e0.copy(child = xml.Text(text))
      }
      TSpan(id = id, y = y, x = x, text = text, node = e)

    case _ => sys.error(s"Not a tspan: $n")
  }

  def parseText(n: xml.Node): Option[Text] = n match {
    case e: xml.Elem if e.label == "text" =>
      val id        = (e \ "@id").text
      val style     = parseStyle     (e \ "@style")
      val transform = parseTransform (e \ "@transform")
      val children  = (e \ "tspan").map(parseTSpan)
      val e1        = e.copy(child = children.map(_.node))
      val res       = Text(id = id, style = style, transform = transform, children = children, node = e1)
      Some(res)

    case _ => None // sys.error(s"Could not parse text $n")
  }

  def run(): Unit = {
    val dir     = file("/") / "data" / "projects" / "Schwaermen" / "catalog"
    val fOutCat = dir / "schwaermen_cat_hh_cat.pdf"
    val fOutAll = parDe.zipWithIndex.map { case (text, i) =>
      val textId = i + 1
      println(s"RENDERING $textId")
      run(text = text, textId = textId)
    }
    require (dir.isDirectory, s"Not a directory: $dir")
    val argsCat = fOutAll.map(_.path) :+ "cat" :+ "output" :+ fOutCat.path
    exec(pdftk, dir, argsCat)
  }

  def run(text: String, textId: Int): File = {
    val latex = latexTemplate.format(text)
    val dir   = file("/") / "data" / "temp" / "latex"
    require (dir.isDirectory, s"Not a directory: $dir")
    val fOutTex = dir / s"schwaermen_cat_hh_$textId.tex"
    val fOutPDF = fOutTex.replaceExt("pdf")
    writeText(latex, fOutTex)

    val argPDF = Seq("-interaction=batchmode", fOutTex.path)
    exec(pdflatex, dir, argPDF)

    val fOutSVG = fOutPDF.replaceExt("svg")
    val argSVG = Seq("-l", fOutSVG.path, fOutPDF.path)
    exec(inkscape, dir, argSVG)

    /*

    <?xml version="1.0" encoding="UTF-8" standalone="no"?>
    <svg viewBox="0 0 755.90533 755.90533" height="755.90533" width="755.90533">
      <g transform="matrix(1.3333333,0,0,-1.3333333,0,755.90533)">
        <text id="text22"
          style="font-variant:normal;font-weight:normal;font-size:8.46819973px;font-family:Alegreya;-inkscape-font-specification:Alegreya-Regular;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
          transform="matrix(1,0,0,-1,70.866,486.1)">

          <tspan id="tspan12"
            y="0"
            x="0 5.0047064 11.118747 15.640765 19.586946 24.346075 28.893497 32.687252 34.753494 40.249355 43.653572 46.041603 49.632122 56.863964 61.445259 71.200623 74.994377 77.382408 84.842896 89.847603 93.869995 98.341209 102.13496 105.37828 107.76631 114.81186 118.60561 123.36474 126.15924 130.88451 135.69444 141.66452 145.45827 150.55612 157.60167 161.39542 166.15456 169.55876 173.14929 177.67131 179.73755 182.12558 185.71609 190.23811 196.73322 201.62784 204.87117"
            >EshandeltsichumeinExperiment.ZweimenschlicheAr-</tspan>

          ...
        </text>
        <text id="text26"
          style="font-style:italic;font-variant:normal;font-size:8.46819973px;font-family:Alegreya;-inkscape-font-specification:Alegreya-Italic;writing-mode:lr-tb;fill:#000000;fill-opacity:1;fill-rule:nonzero;stroke:none"
          transform="matrix(1,0,0,-1,124.166,445.453)">

          ...
        </text>
      </g>
    </svg>

     */

    val svgDoc  : xml.Elem    = xml.XML.loadFile(fOutSVG)
    val group   : xml.Elem    = (svgDoc \ "g").head.asInstanceOf[xml.Elem]
//    val textSeq : xml.NodeSeq = group  \ "text"

//    println(s"textSeq.size = ${textSeq.size}")  // 5 : regular, italics, regular, italics, regular

    val groupOut: xml.Node = {
      val tt0         = group.child.flatMap(parseText)
      val (th, tt1) = {
        val ttx             = tt0.head
        val y0              = ttx.transform(ttx.children.head.start).y
        val (t1, t2)        = tt0.span(t => t.children.forall(c => t.transform(c.start).y == y0))
        val t2h +: t2t      = t2
        val i               = t2h.children.indexWhere(c => t2h.transform(c.start).y != y0)
        val (t3, t4)        = t2h.splitAt(i)
        val _th : Seq[Text] = t1 :+ t3
        val _tt1: Seq[Text] = t4 +: t2t
        (_th, _tt1)
      }

      val (tl, tm) = {
        val ttx             = tt1.last
        val y0              = ttx.transform(ttx.children.last.start).y
        val (t1r, t2r)      = tt1.reverse.span(t => t.children.forall(c => t.transform(c.start).y == y0))
        val t1              = t1r.reverse
        val t2              = t2r.reverse
        val t2i :+ t2l      = t2
        val i               = t2l.children.lastIndexWhere(c => t2l.transform(c.start).y != y0) + 1
        val (t4, t3)        = t2l.splitAt(i)
        val _tl: Seq[Text]  = t1  :+ t3
        val _tm: Seq[Text]  = t2i :+ t4
        (_tl, _tm)
      }

      def fade(in: Seq[Text], dir: Boolean): Seq[Text] =
        in.flatMap { t =>
          val flat = t.children.indices.flatMap { i =>
            t.chop(i)
          }
          val n = flat.size
          flat.zipWithIndex.map { case (_t, ti) =>
            val t0  = _t.transform
            val at0 = t0.toAwt
            import numbers.Implicits._
            val sy  =
              if (dir) (ti + 1).linlin(0, n, 0.0, 1.0)
              else      ti     .linlin(0, n, 1.0, 0.0)
            val at1 = AffineTransform.getScaleInstance(1.0, sy)
            // we have to compensate for the y coordinate inside the tspan
            val comp0 = _t.children.head.y * (1 - sy)
            val comp = if (dir) comp0 else comp0 - 4.0 * (1 - sy)
            at0.translate(0, comp)
            at0.concatenate(at1)
            val t1  = Transform.fromAwt(at0)
//            val t1 = t0.copy(d = t0.d * -sy, f = 733 - t0.f) // - t0.f * sy / 1.33333)
            _t.setTransform(t1)
          }
        }

      val thI = fade(th, dir = true )
      val tlI = fade(tl, dir = false)

      group.copy(child = (thI ++ tm ++ tlI).map(_.node))
    }

    val svgDocCOut: Seq[xml.Node] = svgDoc.child.map {
      case `group` =>
//        println("GROUP-OUT")
        groupOut
      case x =>
//        println(s"OTHER: $x")
        x
    }
    val svgDocOut = svgDoc.copy(child = svgDocCOut)

    val fOutSVG2 = fOutSVG.parent / s"${fOutSVG.base}-out.svg"
    // XXX TODO --- pretty printing fails because it puts newlines before tspan text
    writePretty(svgDocOut, fOutSVG2, pretty = false)
//    val writer = new FileWriter(fOutSVG2)
//    try {
//      xml.XML.write(writer, node = svgDocOut, enc = "", xmlDecl = false, doctype = null)
//    } finally {
//      writer.close()
//    }

    val fOutPDF2 = fOutSVG2.replaceExt("pdf")
    val argsSVG2 = Seq("--export-pdf", fOutPDF2.path, fOutSVG2.path)
    exec(inkscape, dir, argsSVG2)

    fOutPDF2
  }

  def exec(program: String, dir: File, args: Seq[String]): Unit = {
    import sys.process._
    val cmd = program +: args
    val res = Process(cmd, dir).!
    require (res == 0, s"$program, failed with code $res")
  }

  def writePretty(node: xml.Node, f: File, pretty: Boolean = true): Unit = {
    val pp      = new xml.PrettyPrinter(80, 2)
//    {
//      override protected def traverse(node: xml.Node, pScope: xml.NamespaceBinding, ind: Int): Unit =
//        node match {
//          case xml.Text(s) if s.trim() != "" => super.traverse(node, pScope, 0)
//          case _ => super.traverse(node, pScope, ind)
//        }
//    }
    val fos     = new FileOutputStream(f)
    val writer  = Channels.newWriter(fos.getChannel, "UTF-8")

    try {
      writer.write("<?xml version='1.0' encoding='UTF-8'?>\n")
      val s = if (pretty) pp.format(node) else node.toString()
      writer.write(s)
    } finally {
      writer.close()
    }
  }
}