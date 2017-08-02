package de.sciss.schwaermen

import de.sciss.file._
import de.sciss.fscape.Graph
import de.sciss.fscape.stream.Control
import de.sciss.span.Span
import de.sciss.synth.io.{AudioFile, AudioFileSpec}

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

object BuildSimilarities {
  case class Word(index: Int, span: Span, text: String) {
    // override def toString = s"$productPrefix($index, $span, ${quote(text)})"
  }

  case class Vertex(words: List[Word]) {
    def span: Span = Span(words.head.span.start, words.last.span.stop)

    def index: Int = words.head.index
  }

  val vertices: List[Vertex] = List(Vertex(
    List(Word(0, Span(91745,139373), "da"))
  ), Vertex(
    List(Word(1, Span(191788,243605), "dreht"))
  ), Vertex(
    List(Word(2, Span(262717,326354), "sich"))
  ), Vertex(
    List(Word(3, Span(359575,398206), "der"), Word(4, Span(398206,446363), "baum"))
  ), Vertex(
    List(Word(4, Span(398206,446363), "baum"))
  ), Vertex(
    List(Word(5, Span(543226,585165), "sich"))
  ), Vertex(
    List(Word(6, Span(599424,630118), "dreht"), Word(7, Span(630118,654725), "er"))
  ), Vertex(
    List(Word(7, Span(630118,654725), "er"))
  ), Vertex(
    List(Word(8, Span(683856,715344), "und"))
  ), Vertex(
    List(Word(9, Span(716411,746831), "dreht"))
  ), Vertex(
    List(Word(10, Span(803158,827457), "er"), Word(11, Span(827457,848316), "und"))
  ), Vertex(
    List(Word(11, Span(827457,848316), "und"))
  ), Vertex(
    List(Word(12, Span(890299,913452), "mit"))
  ), Vertex(
    List(Word(13, Span(958977,984732), "dreht"), Word(14, Span(984732,1008590), "es"))
  ), Vertex(
    List(Word(14, Span(984732,1008590), "es"))
  ), Vertex(
    List(Word(15, Span(1096706,1126253), "mich"))
  ), Vertex(
    List(Word(16, Span(1200158,1230058), "ein"))
  ), Vertex(
    List(Word(17, Span(1349321,1389143), "und"))
  ), Vertex(
    List(Word(18, Span(1451869,1472993), "die"), Word(19, Span(1472993,1522182), "blüten"))
  ), Vertex(
    List(Word(19, Span(1472993,1522182), "blüten"))
  ), Vertex(
    List(Word(20, Span(1585724,1628060), "mit"))
  ), Vertex(
    List(Word(21, Span(1634175,1673115), "ihm"))
  ), Vertex(
    List(Word(22, Span(1747378,1771942), "und"))
  ), Vertex(
    List(Word(23, Span(1786551,1825138), "ich"))
  ), Vertex(
    List(Word(24, Span(1912352,1947058), "mit"))
  ), Vertex(
    List(Word(25, Span(2004928,2036415), "der"), Word(26, Span(2036415,2092202), "erde"))
  ), Vertex(
    List(Word(26, Span(2036415,2092202), "erde"))
  ), Vertex(
    List(Word(27, Span(2186839,2210124), "den"), Word(28, Span(2210124,2254180), "tisch"))
  ), Vertex(
    List(Word(28, Span(2210124,2254180), "tisch"))
  ), Vertex(
    List(Word(29, Span(2326615,2349679), "dem"), Word(30, Span(2349679,2393823), "netz"))
  ), Vertex(
    List(Word(30, Span(2349679,2393823), "netz"))
  ), Vertex(
    List(Word(31, Span(2488875,2516835), "und"))
  ), Vertex(
    List(Word(32, Span(2575591,2601213), "der"), Word(33, Span(2601213,2652546), "großen"))
  ), Vertex(
    List(Word(34, Span(2694884,2733295), "zahl"))
  ), Vertex(
    List(Word(35, Span(2734663,2777528), "sporen"))
  ), Vertex(
    List(Word(36, Span(2901168,2925997), "vor"), Word(37, Span(2925997,2950296), "mir"))
  ), Vertex(
    List(Word(38, Span(3048988,3095073), "ist"))
  ), Vertex(
    List(Word(39, Span(3136908,3184183), "nichts"))
  ), Vertex(
    List(Word(40, Span(3233489,3321468), "übersehbar"))
  ), Vertex(
    List(Word(41, Span(3441181,3466406), "und"), Word(42, Span(3466406,3513329), "nichts"))
  ), Vertex(
    List(Word(42, Span(3466406,3513329), "nichts"))
  ), Vertex(
    List(Word(43, Span(3570130,3640690), "überschaubar"))
  ), Vertex(
    List(Word(44, Span(3676873,3700378), "vor"), Word(45, Span(3700378,3723045), "mir"))
  ), Vertex(
    List(Word(46, Span(3784608,3805820), "so"), Word(47, Span(3805820,3838189), "groß"))
  ), Vertex(
    List(Word(47, Span(3805820,3838189), "groß"))
  ), Vertex(
    List(Word(48, Span(3929699,3957658), "geht"))
  ), Vertex(
    List(Word(49, Span(3962763,3997866), "auf"))
  ), Vertex(
    List(Word(50, Span(4001439,4021416), "das"), Word(51, Span(4021416,4064722), "große"))
  ), Vertex(
    List(Word(52, Span(4182012,4202606), "und"), Word(53, Span(4202606,4225979), "vor"), Word(54, Span(4225979,4245913), "mir"))
  ), Vertex(
    List(Word(53, Span(4202606,4225979), "vor"), Word(54, Span(4225979,4245913), "mir"))
  ), Vertex(
    List(Word(55, Span(4340150,4362332), "immer"), Word(56, Span(4362332,4384382), "mehr"))
  ), Vertex(
    List(Word(57, Span(4439013,4458020), "und"), Word(58, Span(4458020,4486465), "mehr"))
  ), Vertex(
    List(Word(59, Span(4597461,4615587), "geht"), Word(60, Span(4615587,4630492), "mir"), Word(61, Span(4630492,4664890), "davon"))
  ), Vertex(
    List(Word(60, Span(4615587,4630492), "mir"), Word(61, Span(4630492,4664890), "davon"))
  ), Vertex(
    List(Word(61, Span(4630492,4664890), "davon"))
  ), Vertex(
    List(Word(62, Span(4745328,4758955), "was"), Word(63, Span(4758955,4784712), "nicht"))
  ), Vertex(
    List(Word(63, Span(4758955,4784712), "nicht"))
  ), Vertex(
    List(Word(64, Span(4796570,4806184), "in"), Word(65, Span(4806184,4819855), "das"), Word(66, Span(4819855,4849181), "innen"))
  ), Vertex(
    List(Word(66, Span(4819855,4849181), "innen"))
  ), Vertex(
    List(Word(67, Span(4936025,4996574), "hineinvergeht"))
  ), Vertex(
    List(Word(68, Span(5083974,5131117), "drängt"))
  ), Vertex(
    List(Word(69, Span(5164508,5176327), "aus"), Word(70, Span(5176327,5188499), "den"), Word(71, Span(5188499,5240757), "poren"))
  ), Vertex(
    List(Word(72, Span(5313731,5349319), "drängt"))
  ), Vertex(
    List(Word(73, Span(5411982,5440647), "und"), Word(74, Span(5440647,5474692), "drängt"))
  ), Vertex(
    List(Word(74, Span(5440647,5474692), "drängt"))
  ), Vertex(
    List(Word(75, Span(5555029,5590618), "sprießt"), Word(76, Span(5590618,5614917), "sich"))
  ), Vertex(
    List(Word(76, Span(5590618,5614917), "sich"))
  ), Vertex(
    List(Word(77, Span(5678538,5702396), "ins"), Word(78, Span(5702396,5755624), "blattgrün"))
  ), Vertex(
    List(Word(78, Span(5702396,5755624), "blattgrün"))
  ), Vertex(
    List(Word(79, Span(5868550,5904139), "verrennt"))
  ), Vertex(
    List(Word(80, Span(5905462,5933597), "sich"))
  ), Vertex(
    List(Word(81, Span(6001443,6015158), "in"), Word(82, Span(6015158,6068620), "rinden"))
  ), Vertex(
    List(Word(83, Span(6129035,6146719), "legen"), Word(84, Span(6146719,6172517), "ab"))
  ), Vertex(
    List(Word(85, Span(6175673,6192695), "sich"), Word(86, Span(6192695,6225550), "larven"))
  ), Vertex(
    List(Word(86, Span(6192695,6225550), "larven"))
  ), Vertex(
    List(Word(87, Span(6302078,6322055), "legen"), Word(88, Span(6322055,6334315), "sich"), Word(89, Span(6334315,6348339), "in"), Word(90, Span(6348339,6366155), "mich"))
  ), Vertex(
    List(Word(88, Span(6322055,6334315), "sich"), Word(89, Span(6334315,6348339), "in"), Word(90, Span(6348339,6366155), "mich"))
  ), Vertex(
    List(Word(91, Span(6496816,6534830), "geben"))
  ), Vertex(
    List(Word(92, Span(6568349,6581844), "ein"), Word(93, Span(6581844,6625282), "zeugnis"))
  ), Vertex(
    List(Word(93, Span(6581844,6625282), "zeugnis"))
  ), Vertex(
    List(Word(94, Span(6700420,6715767), "vom"), Word(95, Span(6715767,6748357), "kommen"))
  ), Vertex(
    List(Word(95, Span(6715767,6748357), "kommen"))
  ), Vertex(
    List(Word(96, Span(6862848,6877931), "vom"), Word(97, Span(6877931,6900598), "gehen"))
  ), Vertex(
    List(Word(98, Span(6973953,6984405), "so"), Word(99, Span(6984405,7017832), "grotesk"))
  ), Vertex(
    List(Word(99, Span(6984405,7017832), "grotesk"))
  ), Vertex(
    List(Word(100, Span(7084929,7095866), "wie"), Word(101, Span(7095866,7108126), "ihr"), Word(102, Span(7108126,7154651), "tanz"))
  ), Vertex(
    List(Word(101, Span(7095866,7108126), "ihr"), Word(102, Span(7108126,7154651), "tanz"))
  ), Vertex(
    List(Word(102, Span(7108126,7154651), "tanz"))
  ), Vertex(
    List(Word(103, Span(7204784,7222027), "so"), Word(104, Span(7222027,7266215), "heimelts"))
  ), Vertex(
    List(Word(104, Span(7222027,7266215), "heimelts"))
  ), Vertex(
    List(Word(105, Span(7317471,7344460), "mich"))
  ), Vertex(
    List(Word(106, Span(7363423,7393367), "an"))
  ), Vertex(
    List(Word(107, Span(7472084,7492502), "und"), Word(108, Span(7492502,7506879), "so"), Word(109, Span(7506879,7548818), "grenzts"))
  ), Vertex(
    List(Word(108, Span(7492502,7506879), "so"), Word(109, Span(7506879,7548818), "grenzts"))
  ), Vertex(
    List(Word(109, Span(7506879,7548818), "grenzts"))
  ), Vertex(
    List(Word(110, Span(7613232,7637663), "mich"))
  ), Vertex(
    List(Word(111, Span(7650042,7679853), "aus"))
  ), Vertex(
    List(Word(112, Span(7778123,7812918), "weiß"))
  ), Vertex(
    List(Word(113, Span(7815167,7841803), "mich"))
  ), Vertex(
    List(Word(114, Span(7902593,7921203), "und"), Word(115, Span(7921203,7963760), "nichts"))
  ), Vertex(
    List(Word(115, Span(7921203,7963760), "nichts"))
  ), Vertex(
    List(Word(116, Span(8005904,8036113), "weiß"))
  ), Vertex(
    List(Word(117, Span(8039994,8069056), "mich"))
  ), Vertex(
    List(Word(118, Span(8144085,8164503), "und"), Word(119, Span(8164503,8204016), "nichts"))
  ), Vertex(
    List(Word(119, Span(8164503,8204016), "nichts"))
  ), Vertex(
    List(Word(120, Span(8285496,8326332), "weiß"))
  ), Vertex(
    List(Word(121, Span(8362513,8382623), "mein"), Word(122, Span(8382623,8424518), "mikriger"), Word(123, Span(8424518,8470249), "körper"))
  ), Vertex(
    List(Word(122, Span(8382623,8424518), "mikriger"), Word(123, Span(8424518,8470249), "körper"))
  ), Vertex(
    List(Word(123, Span(8424518,8470249), "körper"))
  ), Vertex(
    List(Word(124, Span(8526050,8540250), "im"), Word(125, Span(8540250,8604988), "überwurf"))
  ), Vertex(
    List(Word(125, Span(8540250,8604988), "überwurf"))
  ), Vertex(
    List(Word(126, Span(8655282,8665381), "zu"), Word(127, Span(8665381,8691385), "seinem"), Word(128, Span(8691385,8737999), "verhältnis"))
  ), Vertex(
    List(Word(128, Span(8691385,8737999), "verhältnis"))
  ), Vertex(
    List(Word(129, Span(8790675,8833319), "gespannt"))
  ), Vertex(
    List(Word(130, Span(8907841,8917852), "und"), Word(131, Span(8917852,8931964), "so"), Word(132, Span(8931964,8954940), "dünn"))
  ), Vertex(
    List(Word(131, Span(8917852,8931964), "so"), Word(132, Span(8931964,8954940), "dünn"))
  ), Vertex(
    List(Word(133, Span(9013434,9031779), "seinem"), Word(134, Span(9031779,9054579), "sein"), Word(135, Span(9054579,9101104), "überlassen"))
  ), Vertex(
    List(Word(135, Span(9054579,9101104), "überlassen"))
  ), Vertex(
    List(Word(136, Span(9191824,9203158), "mit"), Word(137, Span(9203158,9220313), "sich"), Word(138, Span(9220313,9249375), "rein"))
  ), Vertex(
    List(Word(137, Span(9203158,9220313), "sich"), Word(138, Span(9220313,9249375), "rein"))
  ), Vertex(
    List(Word(138, Span(9220313,9249375), "rein"))
  ), Vertex(
    List(Word(139, Span(9337558,9347084), "und"), Word(140, Span(9347084,9352155), "im"), Word(141, Span(9352155,9386597), "unreinen"), Word(142, Span(9386597,9428095), "zugleich"))
  ), Vertex(
    List(Word(141, Span(9352155,9386597), "unreinen"), Word(142, Span(9386597,9428095), "zugleich"))
  ), Vertex(
    List(Word(142, Span(9386597,9428095), "zugleich"))
  ), Vertex(
    List(Word(143, Span(9499443,9514569), "sieht"), Word(144, Span(9514569,9529078), "sich"), Word(145, Span(9529078,9555626), "langsam"), Word(146, Span(9555626,9591877), "vergehen"))
  ), Vertex(
    List(Word(145, Span(9529078,9555626), "langsam"), Word(146, Span(9555626,9591877), "vergehen"))
  ), Vertex(
    List(Word(147, Span(9681343,9697969), "sieht"), Word(148, Span(9697969,9713227), "nicht"), Word(149, Span(9713227,9730911), "mehr"), Word(150, Span(9730911,9740084), "das"), Word(151, Span(9740084,9778760), "darüber"))
  ), Vertex(
    List(Word(148, Span(9697969,9713227), "nicht"), Word(149, Span(9713227,9730911), "mehr"), Word(150, Span(9730911,9740084), "das"), Word(151, Span(9740084,9778760), "darüber"))
  ), Vertex(
    List(Word(150, Span(9730911,9740084), "das"), Word(151, Span(9740084,9778760), "darüber"))
  ), Vertex(
    List(Word(151, Span(9740084,9778760), "darüber"))
  ), Vertex(
    List(Word(152, Span(9899638,9936153), "umfangen"), Word(153, Span(9936153,9985544), "hundsrosen"))
  ), Vertex(
    List(Word(153, Span(9936153,9985544), "hundsrosen"))
  ), Vertex(
    List(Word(154, Span(10040716,10081818), "gemeine"))
  ), Vertex(
    List(Word(155, Span(10143229,10163647), "alle"), Word(156, Span(10163647,10198662), "türen"))
  ), Vertex(
    List(Word(156, Span(10163647,10198662), "türen"))
  ), Vertex(
    List(Word(157, Span(10245106,10254984), "für"), Word(158, Span(10254984,10300002), "auswüchse"))
  ), Vertex(
    List(Word(158, Span(10254984,10300002), "auswüchse"))
  ), Vertex(
    List(Word(159, Span(10359906,10412826), "verführen"))
  ), Vertex(
    List(Word(160, Span(10469228,10476593), "in"), Word(161, Span(10476593,10485589), "eine"), Word(162, Span(10485589,10520869), "verheißung"), Word(163, Span(10520869,10553062), "hinein"))
  ), Vertex(
    List(Word(164, Span(10612207,10634698), "rosa"), Word(165, Span(10634698,10678842), "blutschwämme"))
  ), Vertex(
    List(Word(165, Span(10634698,10678842), "blutschwämme"))
  ), Vertex(
    List(Word(166, Span(10715943,10729041), "mit"), Word(167, Span(10729041,10757574), "knospen"), Word(168, Span(10757574,10796013), "übersät"))
  ), Vertex(
    List(Word(167, Span(10729041,10757574), "knospen"), Word(168, Span(10757574,10796013), "übersät"))
  ), Vertex(
    List(Word(168, Span(10757574,10796013), "übersät"))
  ), Vertex(
    List(Word(169, Span(10867257,10896804), "vergeht"), Word(170, Span(10896804,10904874), "der"), Word(171, Span(10904874,10933627), "baum"))
  ), Vertex(
    List(Word(170, Span(10896804,10904874), "der"), Word(171, Span(10904874,10933627), "baum"))
  ), Vertex(
    List(Word(171, Span(10904874,10933627), "baum"))
  ), Vertex(
    List(Word(172, Span(11005064,11015868), "von"), Word(173, Span(11015868,11042858), "einem"))
  ), Vertex(
    List(Word(174, Span(11094355,11105247), "auf"), Word(175, Span(11105247,11114817), "das"), Word(176, Span(11114817,11143658), "andere"))
  ), Vertex(
    List(Word(175, Span(11105247,11114817), "das"), Word(176, Span(11114817,11143658), "andere"))
  ), Vertex(
    List(Word(176, Span(11114817,11143658), "andere"))
  ), Vertex(
    List(Word(177, Span(11180531,11213334), "trifft"), Word(178, Span(11213334,11219949), "der"), Word(179, Span(11219949,11250025), "schlag"))
  ), Vertex(
    List(Word(178, Span(11213334,11219949), "der"), Word(179, Span(11219949,11250025), "schlag"))
  ), Vertex(
    List(Word(180, Span(11306702,11320196), "auf"), Word(181, Span(11320196,11339556), "ihn"))
  ), Vertex(
    List(Word(182, Span(11340410,11367119), "zu"))
  ), Vertex(
    List(Word(183, Span(11425676,11436127), "hat"), Word(184, Span(11436127,11451474), "keine"), Word(185, Span(11451474,11481109), "kraft"))
  ), Vertex(
    List(Word(184, Span(11436127,11451474), "keine"), Word(185, Span(11451474,11481109), "kraft"))
  ), Vertex(
    List(Word(185, Span(11451474,11481109), "kraft"))
  ), Vertex(
    List(Word(186, Span(11505718,11517537), "zum"), Word(187, Span(11517537,11548098), "fallen"))
  ), Vertex(
    List(Word(188, Span(11609902,11624984), "steht"), Word(189, Span(11624984,11640595), "lange"), Word(190, Span(11640595,11647960), "noch"), Word(191, Span(11647960,11675522), "scheinbar"), Word(192, Span(11675522,11680814), "im"), Word(193, Span(11680814,11707709), "saft"))
  ), Vertex(
    List(Word(191, Span(11647960,11675522), "scheinbar"), Word(192, Span(11675522,11680814), "im"), Word(193, Span(11680814,11707709), "saft"))
  ), Vertex(
    List(Word(193, Span(11680814,11707709), "saft"))
  ), Vertex(
    List(Word(194, Span(11806550,11823397), "nichts"), Word(195, Span(11823397,11830805), "mehr"), Word(196, Span(11830805,11878213), "austragend"))
  ), Vertex(
    List(Word(196, Span(11830805,11878213), "austragend"))
  ), Vertex(
    List(Word(197, Span(11987468,11995759), "noch"), Word(198, Span(11995759,12029672), "umstachelt"), Word(199, Span(12029672,12043166), "sein"), Word(200, Span(12043166,12096442), "fleisch"))
  ), Vertex(
    List(Word(198, Span(11995759,12029672), "umstachelt"), Word(199, Span(12029672,12043166), "sein"), Word(200, Span(12043166,12096442), "fleisch"))
  ), Vertex(
    List(Word(200, Span(12043166,12096442), "fleisch"))
  ), Vertex(
    List(Word(201, Span(12160352,12184210), "spät"), Word(202, Span(12184210,12192325), "zur"), Word(203, Span(12192325,12232565), "stunde"))
  ), Vertex(
    List(Word(202, Span(12184210,12192325), "zur"), Word(203, Span(12192325,12232565), "stunde"))
  ), Vertex(
    List(Word(204, Span(12279908,12293976), "lässt"), Word(205, Span(12293976,12307779), "sich"), Word(206, Span(12307779,12352805), "ansehen"))
  ), Vertex(
    List(Word(206, Span(12307779,12352805), "ansehen"))
  ), Vertex(
    List(Word(207, Span(12520577,12539760), "dass"), Word(208, Span(12539760,12552946), "an"), Word(209, Span(12552946,12576892), "ihm"))
  ), Vertex(
    List(Word(208, Span(12539760,12552946), "an"), Word(209, Span(12552946,12576892), "ihm"))
  ), Vertex(
    List(Word(209, Span(12552946,12576892), "ihm"))
  ), Vertex(
    List(Word(210, Span(12674738,12694274), "sich"), Word(211, Span(12694274,12732112), "nichts"))
  ), Vertex(
    List(Word(211, Span(12694274,12732112), "nichts"))
  ), Vertex(
    List(Word(212, Span(12774872,12808300), "mehr"))
  ), Vertex(
    List(Word(213, Span(12874037,12918181), "bewegt"))
  ), Vertex(
    List(Word(214, Span(13088686,13103548), "so"), Word(215, Span(13103548,13141222), "schief"))
  ), Vertex(
    List(Word(215, Span(13103548,13141222), "schief"))
  ), Vertex(
    List(Word(216, Span(13189840,13201968), "und"), Word(217, Span(13201968,13211493), "so"), Word(218, Span(13211493,13251649), "steil"))
  ), Vertex(
    List(Word(218, Span(13211493,13251649), "steil"))
  ), Vertex(
    List(Word(219, Span(13318888,13333264), "soll"), Word(220, Span(13333264,13344598), "sich"), Word(221, Span(13344598,13352580), "der"), Word(222, Span(13352580,13387154), "weg"))
  ), Vertex(
    List(Word(221, Span(13344598,13352580), "der"), Word(222, Span(13352580,13387154), "weg"))
  ), Vertex(
    List(Word(223, Span(13431256,13448851), "nicht"), Word(224, Span(13448851,13494054), "entpuppen"))
  ), Vertex(
    List(Word(224, Span(13448851,13494054), "entpuppen"))
  ), Vertex(
    List(Word(225, Span(13577589,13588217), "wie"), Word(226, Span(13588217,13600785), "er"), Word(227, Span(13600785,13627863), "sich"))
  ), Vertex(
    List(Word(228, Span(13668736,13715129), "anschickt"))
  ), Vertex(
    List(Word(229, Span(13717863,13727742), "zu"), Word(230, Span(13727742,13752482), "sein"))
  ), Vertex(
    List(Word(231, Span(13881412,13921719), "verpasse"), Word(232, Span(13921719,13943637), "ihm"))
  ), Vertex(
    List(Word(233, Span(13999632,14062607), "glücksstreifen"))
  ), Vertex(
    List(Word(234, Span(14150646,14173357), "male"), Word(235, Span(14173357,14219152), "sie"))
  ), Vertex(
    List(Word(236, Span(14235742,14278783), "eigenhändig"), Word(237, Span(14278783,14299201), "an"))
  ), Vertex(
    List(Word(238, Span(14350863,14371282), "zum"), Word(239, Span(14371282,14396198), "zeichen"), Word(240, Span(14396198,14405459), "dass"), Word(241, Span(14405459,14421969), "ich"))
  ), Vertex(
    List(Word(240, Span(14396198,14405459), "dass"), Word(241, Span(14405459,14421969), "ich"))
  ), Vertex(
    List(Word(242, Span(14468154,14491395), "bin"))
  ), Vertex(
    List(Word(243, Span(14585536,14616891), "nimm"))
  ), Vertex(
    List(Word(244, Span(14671962,14707772), "sagt"))
  ), Vertex(
    List(Word(245, Span(14713080,14720444), "der"), Word(246, Span(14720444,14765250), "überbringer"))
  ), Vertex(
    List(Word(246, Span(14720444,14765250), "überbringer"))
  ), Vertex(
    List(Word(247, Span(14812010,14825989), "was"), Word(248, Span(14825989,14852405), "du"))
  ), Vertex(
    List(Word(248, Span(14825989,14852405), "du"))
  ), Vertex(
    List(Word(249, Span(14888081,14939722), "bekommst"))
  ), Vertex(
    List(Word(250, Span(14991434,14999063), "und"), Word(251, Span(14999063,15034387), "übergibt"))
  ), Vertex(
    List(Word(251, Span(14999063,15034387), "übergibt"))
  ), Vertex(
    List(Word(252, Span(15036328,15058113), "mir"))
  ), Vertex(
    List(Word(253, Span(15126539,15146870), "seine"), Word(254, Span(15146870,15186543), "stafette"))
  ), Vertex(
    List(Word(255, Span(15279651,15296894), "ich"), Word(256, Span(15296894,15330586), "laufe"))
  ), Vertex(
    List(Word(257, Span(15382692,15409373), "mit"))
  ), Vertex(
    List(Word(258, Span(15474957,15511042), "und"), Word(259, Span(15511042,15562053), "weiter"))
  ), Vertex(
    List(Word(260, Span(15627307,15652708), "und"))
  ), Vertex(
    List(Word(261, Span(15689328,15715744), "mit"))
  ), Vertex(
    List(Word(262, Span(15780013,15812780), "und"))
  ), Vertex(
    List(Word(263, Span(15814544,15889910), "fortwärts"))
  ), Vertex(
    List(Word(264, Span(15935327,15950761), "zur"), Word(265, Span(15950761,16017793), "netzfalle"))
  ), Vertex(
    List(Word(265, Span(15950761,16017793), "netzfalle"))
  ), Vertex(
    List(Word(266, Span(16083468,16102960), "von"), Word(267, Span(16102960,16147986), "allein"))
  ), Vertex(
    List(Word(267, Span(16102960,16147986), "allein"))
  ), Vertex(
    List(Word(268, Span(16212895,16231373), "den"), Word(269, Span(16231373,16276444), "laufenden"))
  ), Vertex(
    List(Word(269, Span(16231373,16276444), "laufenden"))
  ), Vertex(
    List(Word(270, Span(16327036,16368094), "kosten"))
  ), Vertex(
    List(Word(271, Span(16371842,16424100), "davon"))
  ), Vertex(
    List(Word(272, Span(16509756,16522634), "und"), Word(273, Span(16522634,16548432), "den"), Word(274, Span(16548432,16591209), "laufenden"))
  ), Vertex(
    List(Word(273, Span(16522634,16548432), "den"), Word(274, Span(16548432,16591209), "laufenden"))
  ), Vertex(
    List(Word(274, Span(16548432,16591209), "laufenden"))
  ), Vertex(
    List(Word(275, Span(16627762,16666394), "kindern"))
  ), Vertex(
    List(Word(276, Span(16725114,16741739), "und"), Word(277, Span(16741739,16767538), "den"))
  ), Vertex(
    List(Word(278, Span(16802789,16850109), "laufenden"))
  ), Vertex(
    List(Word(279, Span(16852931,16890813), "nasen"))
  ), Vertex(
    List(Word(280, Span(16952029,16969052), "und"), Word(281, Span(16969052,16983296), "dem"), Word(282, Span(16983296,17023868), "laufenden"))
  ), Vertex(
    List(Word(281, Span(16969052,16983296), "dem"), Word(282, Span(16983296,17023868), "laufenden"))
  ), Vertex(
    List(Word(282, Span(16983296,17023868), "laufenden"))
  ), Vertex(
    List(Word(283, Span(17026117,17070217), "lärm"))
  ), Vertex(
    List(Word(284, Span(17137197,17155807), "und"), Word(285, Span(17155807,17167626), "den"), Word(286, Span(17167626,17197857), "laufenden"), Word(287, Span(17197857,17238507), "festen"))
  ), Vertex(
    List(Word(285, Span(17155807,17167626), "den"), Word(286, Span(17167626,17197857), "laufenden"), Word(287, Span(17197857,17238507), "festen"))
  ), Vertex(
    List(Word(287, Span(17197857,17238507), "festen"))
  ), Vertex(
    List(Word(288, Span(17319878,17342545), "und"), Word(289, Span(17342545,17359788), "den"), Word(290, Span(17359788,17398111), "läufigen"), Word(291, Span(17398111,17470655), "hunden"))
  ), Vertex(
    List(Word(289, Span(17342545,17359788), "den"), Word(290, Span(17359788,17398111), "läufigen"), Word(291, Span(17398111,17470655), "hunden"))
  ), Vertex(
    List(Word(291, Span(17398111,17470655), "hunden"))
  ), Vertex(
    List(Word(292, Span(17515394,17552364), "und"))
  ), Vertex(
    List(Word(293, Span(17574179,17612943), "räudigen"))
  ), Vertex(
    List(Word(294, Span(17615589,17655323), "füchsen"))
  ), Vertex(
    List(Word(295, Span(17726720,17739906), "den"), Word(296, Span(17739906,17749784), "auf"), Word(297, Span(17749784,17759178), "den"), Word(298, Span(17759178,17784102), "neuesten"), Word(299, Span(17784102,17807475), "stand"), Word(300, Span(17807475,17843064), "gebrachten"), Word(301, Span(17843064,17940437), "tätigkeitsberichten"))
  ), Vertex(
    List(Word(296, Span(17739906,17749784), "auf"), Word(297, Span(17749784,17759178), "den"), Word(298, Span(17759178,17784102), "neuesten"), Word(299, Span(17784102,17807475), "stand"), Word(300, Span(17807475,17843064), "gebrachten"), Word(301, Span(17843064,17940437), "tätigkeitsberichten"))
  ), Vertex(
    List(Word(301, Span(17843064,17940437), "tätigkeitsberichten"))
  ), Vertex(
    List(Word(302, Span(18029348,18066083), "versprödet"), Word(303, Span(18066083,18084120), "sich"), Word(304, Span(18084120,18131042), "aufgeweichte"), Word(305, Span(18131042,18188184), "haut"))
  ), Vertex(
    List(Word(304, Span(18084120,18131042), "aufgeweichte"), Word(305, Span(18131042,18188184), "haut"))
  ), Vertex(
    List(Word(306, Span(18282998,18297375), "kann"), Word(307, Span(18297375,18310561), "sich"), Word(308, Span(18310561,18326084), "nicht"), Word(309, Span(18326084,18348531), "weiter"), Word(310, Span(18348531,18373850), "biegen"))
  ), Vertex(
    List(Word(308, Span(18310561,18326084), "nicht"), Word(309, Span(18326084,18348531), "weiter"), Word(310, Span(18348531,18373850), "biegen"))
  ), Vertex(
    List(Word(309, Span(18326084,18348531), "weiter"), Word(310, Span(18348531,18373850), "biegen"))
  ), Vertex(
    List(Word(310, Span(18348531,18373850), "biegen"))
  ), Vertex(
    List(Word(311, Span(18475889,18497190), "oder"), Word(312, Span(18497190,18535248), "dehnen"))
  ), Vertex(
    List(Word(313, Span(18612227,18636085), "steht"), Word(314, Span(18636085,18651446), "unter"), Word(315, Span(18651446,18708164), "geheimhaltung"))
  ), Vertex(
    List(Word(314, Span(18636085,18651446), "unter"), Word(315, Span(18651446,18708164), "geheimhaltung"))
  ), Vertex(
    List(Word(315, Span(18651446,18708164), "geheimhaltung"))
  ), Vertex(
    List(Word(316, Span(18742810,18759039), "der"), Word(317, Span(18759039,18822896), "ablösungsprozess"), Word(318, Span(18822896,18834538), "wird"), Word(319, Span(18834538,18883269), "geführt"))
  ), Vertex(
    List(Word(318, Span(18822896,18834538), "wird"), Word(319, Span(18834538,18883269), "geführt"))
  ), Vertex(
    List(Word(320, Span(18945639,18962882), "unter"), Word(321, Span(18962882,19015317), "ausschluss"))
  ), Vertex(
    List(Word(321, Span(18962882,19015317), "ausschluss"))
  ), Vertex(
    List(Word(322, Span(19023174,19072081), "meines"), Word(323, Span(19072081,19160634), "bewusstseins"))
  ), Vertex(
    List(Word(324, Span(19194893,19213460), "hören"), Word(325, Span(19213460,19277659), "einflüsse"))
  ), Vertex(
    List(Word(325, Span(19213460,19277659), "einflüsse"))
  ), Vertex(
    List(Word(326, Span(19294927,19316139), "nicht"), Word(327, Span(19316139,19334529), "auf"), Word(328, Span(19334529,19342908), "zu"), Word(329, Span(19342908,19374087), "sein"))
  ), Vertex(
    List(Word(327, Span(19316139,19334529), "auf"), Word(328, Span(19334529,19342908), "zu"), Word(329, Span(19342908,19374087), "sein"))
  ), Vertex(
    List(Word(328, Span(19334529,19342908), "zu"), Word(329, Span(19342908,19374087), "sein"))
  ), Vertex(
    List(Word(330, Span(19451099,19468915), "legen"), Word(331, Span(19468915,19485276), "sich"), Word(332, Span(19485276,19507987), "mir"))
  ), Vertex(
    List(Word(332, Span(19485276,19507987), "mir"))
  ), Vertex(
    List(Word(333, Span(19535159,19639428), "selbstschmelzend"))
  ), Vertex(
    List(Word(334, Span(19652107,19693296), "nahe"))
  ), Vertex(
    List(Word(335, Span(19745349,19759858), "bis"), Word(336, Span(19759858,19766120), "in"), Word(337, Span(19766120,19775690), "die"), Word(338, Span(19775690,19802458), "poren"))
  ), Vertex(
    List(Word(338, Span(19775690,19802458), "poren"))
  ), Vertex(
    List(Word(339, Span(19857223,19955610), "lindennektarbrisen"))
  ), Vertex(
    List(Word(340, Span(20005000,20087203), "honiggeruch"))
  ), Vertex(
    List(Word(341, Span(20091025,20131024), "heißer"))
  ), Vertex(
    List(Word(342, Span(20162811,20186669), "kriecht"), Word(343, Span(20186669,20194166), "aus"), Word(344, Span(20194166,20207661), "dem"), Word(345, Span(20207661,20239148), "himmel"))
  ), Vertex(
    List(Word(343, Span(20186669,20194166), "aus"), Word(344, Span(20194166,20207661), "dem"), Word(345, Span(20207661,20239148), "himmel"))
  ), Vertex(
    List(Word(345, Span(20207661,20239148), "himmel"))
  ), Vertex(
    List(Word(346, Span(20262002,20332914), "blässlicher"), Word(347, Span(20332914,20365063), "mond"))
  ), Vertex(
    List(Word(348, Span(20411777,20452393), "wirft"), Word(349, Span(20452393,20462889), "sich"), Word(350, Span(20462889,20480088), "kaum"), Word(351, Span(20480088,20497305), "licht"), Word(352, Span(20497305,20523293), "jetzt"))
  ), Vertex(
    List(Word(350, Span(20462889,20480088), "kaum"), Word(351, Span(20480088,20497305), "licht"), Word(352, Span(20497305,20523293), "jetzt"))
  ), Vertex(
    List(Word(353, Span(20525138,20546923), "mir"))
  ), Vertex(
    List(Word(354, Span(20559387,20672856), "ab"))
  ), Vertex(
    List(Word(355, Span(20678379,20729359), "bloße"))
  ), Vertex(
    List(Word(356, Span(20762001,20801823), "augen"))
  ), Vertex(
    List(Word(357, Span(20814279,20854366), "pressen"), Word(358, Span(20854366,20912359), "fünkchen"))
  ), Vertex(
    List(Word(358, Span(20854366,20912359), "fünkchen"))
  ), Vertex(
    List(Word(359, Span(20944115,20960123), "aus"), Word(360, Span(20960123,20981864), "ihren"), Word(361, Span(20981864,21021645), "äpfeln"))
  ), Vertex(
    List(Word(361, Span(20981864,21021645), "äpfeln"))
  ), Vertex(
    List(Word(362, Span(21082571,21096198), "ein"), Word(363, Span(21096198,21134521), "wunder"))
  ), Vertex(
    List(Word(363, Span(21096198,21134521), "wunder"))
  ), Vertex(
    List(Word(364, Span(21177215,21201470), "ist"), Word(365, Span(21201470,21218317), "ihm"), Word(366, Span(21218317,21266596), "geschehen"))
  ), Vertex(
    List(Word(367, Span(21302834,21326295), "und"), Word(368, Span(21326295,21382966), "übersehen"))
  ), Vertex(
    List(Word(368, Span(21326295,21382966), "übersehen"))
  ), Vertex(
    List(Word(369, Span(21428766,21441996), "wird"), Word(370, Span(21441996,21475601), "nichts"))
  ), Vertex(
    List(Word(370, Span(21441996,21475601), "nichts"))
  ), Vertex(
    List(Word(371, Span(21510987,21561129), "eingetragen"))
  ), Vertex(
    List(Word(372, Span(21609405,21626736), "ins"), Word(373, Span(21626736,21664237), "fleisch"))
  ), Vertex(
    List(Word(373, Span(21626736,21664237), "fleisch"))
  ), Vertex(
    List(Word(374, Span(21738244,21783226), "wenn"))
  ), Vertex(
    List(Word(375, Span(21816225,21845066), "was"))
  ), Vertex(
    List(Word(376, Span(21852013,21866390), "so"), Word(377, Span(21866390,21899553), "ist"))
  ), Vertex(
    List(Word(377, Span(21866390,21899553), "ist"))
  ), Vertex(
    List(Word(378, Span(21939825,21954201), "wie"), Word(379, Span(21954201,21982028), "mir"))
  ), Vertex(
    List(Word(380, Span(21985839,22024294), "scheint"))
  ), Vertex(
    List(Word(381, Span(22064331,22111738), "nichts"))
  ), Vertex(
    List(Word(382, Span(22147151,22164041), "ist"), Word(383, Span(22164041,22201923), "bewiesen"))
  ), Vertex(
    List(Word(383, Span(22164041,22201923), "bewiesen"))
  ), Vertex(
    List(Word(384, Span(22261368,22274995), "und"), Word(385, Span(22274995,22282315), "wie"), Word(386, Span(22282315,22326273), "nichts"))
  ), Vertex(
    List(Word(387, Span(22355949,22394095), "existiert"), Word(388, Span(22394095,22423666), "es"))
  ), Vertex(
    List(Word(389, Span(22446579,22457516), "im"), Word(390, Span(22457516,22494894), "nichts"))
  ), Vertex(
    List(Word(391, Span(22571844,22591115), "meine"), Word(392, Span(22591115,22618418), "liebe"))
  ), Vertex(
    List(Word(392, Span(22591115,22618418), "liebe"))
  ), Vertex(
    List(Word(393, Span(22677927,22689084), "muss"), Word(394, Span(22689084,22695743), "ich"), Word(395, Span(22695743,22736007), "machen"))
  ), Vertex(
    List(Word(396, Span(22805750,22823831), "meine"), Word(397, Span(22823831,22857358), "liebe"))
  ), Vertex(
    List(Word(397, Span(22823831,22857358), "liebe"))
  ), Vertex(
    List(Word(398, Span(22889312,22905717), "macht"), Word(399, Span(22905717,22917536), "sich"), Word(400, Span(22917536,22942849), "nicht"))
  ), Vertex(
    List(Word(401, Span(22968814,22984073), "von"), Word(402, Span(22984073,23034170), "allein"))
  ), Vertex(
    List(Word(402, Span(22984073,23034170), "allein"))
  ), Vertex(
    List(Word(403, Span(23100509,23121059), "setze"), Word(404, Span(23121059,23133407), "eine"), Word(405, Span(23133407,23168599), "tat"))
  ), Vertex(
    List(Word(404, Span(23121059,23133407), "eine"), Word(405, Span(23133407,23168599), "tat"))
  ), Vertex(
    List(Word(405, Span(23133407,23168599), "tat"))
  ), Vertex(
    List(Word(406, Span(23199298,23219187), "küsse"), Word(407, Span(23219187,23243951), "dich"))
  ), Vertex(
    List(Word(408, Span(23246171,23284626), "liebe"))
  ), Vertex(
    List(Word(409, Span(23327602,23337171), "dass"), Word(410, Span(23337171,23345374), "du"), Word(411, Span(23345374,23380410), "bist"))
  ), Vertex(
    List(Word(411, Span(23345374,23380410), "bist"))
  ), Vertex(
    List(Word(412, Span(23419184,23448114), "setze"), Word(413, Span(23448114,23461608), "noch"), Word(414, Span(23461608,23474618), "eine"), Word(415, Span(23474618,23510824), "tat"))
  ), Vertex(
    List(Word(414, Span(23461608,23474618), "eine"), Word(415, Span(23474618,23510824), "tat"))
  ), Vertex(
    List(Word(415, Span(23474618,23510824), "tat"))
  ), Vertex(
    List(Word(416, Span(23581491,23592075), "und"), Word(417, Span(23592075,23597032), "die"), Word(418, Span(23597032,23649467), "nächste"))
  ), Vertex(
    List(Word(417, Span(23592075,23597032), "die"), Word(418, Span(23597032,23649467), "nächste"))
  ), Vertex(
    List(Word(419, Span(23682323,23696788), "ganz"), Word(420, Span(23696788,23713811), "ohne"), Word(421, Span(23713811,23750329), "was"))
  ), Vertex(
    List(Word(420, Span(23696788,23713811), "ohne"), Word(421, Span(23713811,23750329), "was"))
  ), Vertex(
    List(Word(421, Span(23713811,23750329), "was"))
  ), Vertex(
    List(Word(422, Span(23781602,23809165), "tätige"), Word(423, Span(23809165,23851633), "taten"))
  ), Vertex(
    List(Word(423, Span(23809165,23851633), "taten"))
  ), Vertex(
    List(Word(424, Span(23879695,23891999), "so"), Word(425, Span(23891999,23907125), "setzt"), Word(426, Span(23907125,23919429), "sich"), Word(427, Span(23919429,23929930), "was"), Word(428, Span(23929930,24016505), "ab"))
  ), Vertex(
    List(Word(429, Span(24106403,24119280), "dass"), Word(430, Span(24119280,24127483), "die"), Word(431, Span(24127483,24189708), "eintagsfliege"))
  ), Vertex(
    List(Word(432, Span(24203768,24218851), "etwa"), Word(433, Span(24218851,24228244), "dann"), Word(434, Span(24228244,24240570), "nicht"), Word(435, Span(24240570,24264384), "mehr"))
  ), Vertex(
    List(Word(436, Span(24360812,24378055), "um"), Word(437, Span(24378055,24398827), "meine"), Word(438, Span(24398827,24434130), "nase"))
  ), Vertex(
    List(Word(438, Span(24398827,24434130), "nase"))
  ), Vertex(
    List(Word(439, Span(24509316,24545433), "ganz"))
  ), Vertex(
    List(Word(440, Span(24546918,24599838), "fahrend"))
  ), Vertex(
    List(Word(441, Span(24630781,24679512), "sich"))
  ), Vertex(
    List(Word(442, Span(24683570,24721981), "auf"))
  ), Vertex(
    List(Word(443, Span(24756865,24769963), "und"), Word(444, Span(24769963,24800438), "fort"))
  ), Vertex(
    List(Word(445, Span(24865823,24880817), "und"), Word(446, Span(24880817,24936038), "umkreisend"))
  ), Vertex(
    List(Word(446, Span(24880817,24936038), "umkreisend"))
  ), Vertex(
    List(Word(447, Span(24986163,25013549), "immer"))
  ), Vertex(
    List(Word(448, Span(25015489,25077105), "umkreisend"))
  ), Vertex(
    List(Word(449, Span(25115530,25133875), "mich"), Word(450, Span(25133875,25189265), "drangsalierend"))
  ), Vertex(
    List(Word(450, Span(25133875,25189265), "drangsalierend"))
  ), Vertex(
    List(Word(451, Span(25244032,25315474), "einfährt"))
  ), Vertex(
    List(Word(452, Span(25334779,25343026), "in"), Word(453, Span(25343026,25355584), "mein"), Word(454, Span(25355584,25367003), "schon"), Word(455, Span(25367003,25414102), "fahriges"))
  ), Vertex(
    List(Word(455, Span(25367003,25414102), "fahriges"))
  ), Vertex(
    List(Word(456, Span(25447024,25503472), "indirektes"))
  ), Vertex(
    List(Word(457, Span(25518124,25554594), "hinein"))
  ), Vertex(
    List(Word(458, Span(25625462,25687157), "pfeilgerade"))
  ), Vertex(
    List(Word(459, Span(25713756,25741980), "zerstört"), Word(460, Span(25741980,25756444), "sich"), Word(461, Span(25756444,25766896), "ihr"), Word(462, Span(25766896,25817170), "faktor"))
  ), Vertex(
    List(Word(463, Span(25858850,25871550), "von"), Word(464, Span(25871550,25910976), "selbst"))
  ), Vertex(
    List(Word(464, Span(25871550,25910976), "selbst"))
  ), Vertex(
    List(Word(465, Span(25940081,25965306), "trennt"))
  ), Vertex(
    List(Word(466, Span(25967070,25979462), "sich"), Word(467, Span(25979462,25986386), "die"), Word(468, Span(25986386,26011472), "wände"))
  ), Vertex(
    List(Word(467, Span(25979462,25986386), "die"), Word(468, Span(25986386,26011472), "wände"))
  ), Vertex(
    List(Word(468, Span(25986386,26011472), "wände"))
  ), Vertex(
    List(Word(469, Span(26058249,26068128), "von"), Word(470, Span(26068128,26076815), "der"), Word(471, Span(26076815,26116638), "krise"))
  ), Vertex(
    List(Word(471, Span(26076815,26116638), "krise"))
  ), Vertex(
    List(Word(472, Span(26156404,26183966), "nimmt"), Word(473, Span(26183966,26196852), "vom"), Word(474, Span(26196852,26247638), "faktischen"))
  ), Vertex(
    List(Word(475, Span(26295325,26340748), "posthum"))
  ), Vertex(
    List(Word(476, Span(26341630,26366259), "nichts"))
  ), Vertex(
    List(Word(477, Span(26368023,26390690), "mit"))
  ), Vertex(
    List(Word(478, Span(26462051,26486394), "nimmt"), Word(479, Span(26486394,26509546), "sich"), Word(480, Span(26509546,26541122), "nur"))
  ), Vertex(
    List(Word(479, Span(26486394,26509546), "sich"), Word(480, Span(26509546,26541122), "nur"))
  ), Vertex(
    List(Word(480, Span(26509546,26541122), "nur"))
  ), Vertex(
    List(Word(481, Span(26609085,26623726), "von"), Word(482, Span(26623726,26652656), "dem"))
  ), Vertex(
    List(Word(483, Span(26701205,26717522), "was"), Word(484, Span(26717522,26726033), "es"), Word(485, Span(26726033,26742111), "nicht"), Word(486, Span(26742111,26817460), "gäbe"))
  ), Vertex(
    List(Word(487, Span(26847677,26857556), "wenn"), Word(488, Span(26857556,26864832), "es"), Word(489, Span(26864832,26888955), "nicht"))
  ), Vertex(
    List(Word(490, Span(26894034,26938751), "einmal"))
  ), Vertex(
    List(Word(491, Span(26975110,26998086), "als"), Word(492, Span(26998086,27044788), "gegeben"))
  ), Vertex(
    List(Word(492, Span(26998086,27044788), "gegeben"))
  ), Vertex(
    List(Word(493, Span(27056901,27110495), "betrachtet"), Word(494, Span(27110495,27139581), "gewesen"), Word(495, Span(27139581,27180382), "wäre"))
  ), Vertex(
    List(Word(494, Span(27110495,27139581), "gewesen"), Word(495, Span(27139581,27180382), "wäre"))
  ), Vertex(
    List(Word(495, Span(27139581,27180382), "wäre"))
  ), Vertex(
    List(Word(496, Span(27266825,27327859), "bröselt"))
  ), Vertex(
    List(Word(497, Span(27357336,27372903), "was"), Word(498, Span(27372903,27392263), "ab"))
  ), Vertex(
    List(Word(499, Span(27440972,27452306), "von"), Word(500, Span(27452306,27458453), "der"), Word(501, Span(27458453,27531334), "fantasie"))
  ), Vertex(
    List(Word(502, Span(27568882,27593622), "sich"))
  ), Vertex(
    List(Word(503, Span(27605704,27640498), "für"))
  ), Vertex(
    List(Word(504, Span(27683242,27708291), "mich"))
  ), Vertex(
    List(Word(505, Span(27808156,27845332), "wird"))
  ), Vertex(
    List(Word(506, Span(27909008,27935512), "erst"), Word(507, Span(27935512,27952818), "jener"), Word(508, Span(27952818,28020556), "sandkuchen"))
  ), Vertex(
    List(Word(507, Span(27935512,27952818), "jener"), Word(508, Span(27952818,28020556), "sandkuchen"))
  ), Vertex(
    List(Word(508, Span(27952818,28020556), "sandkuchen"))
  ), Vertex(
    List(Word(509, Span(28042673,28102473), "interessant"))
  ), Vertex(
    List(Word(510, Span(28155816,28166267), "der"), Word(511, Span(28166267,28187126), "einmal"), Word(512, Span(28187126,28215968), "gebacken"), Word(513, Span(28215968,28227332), "wird"), Word(514, Span(28227332,28257320), "sein"))
  ), Vertex(
    List(Word(511, Span(28166267,28187126), "einmal"), Word(512, Span(28187126,28215968), "gebacken"), Word(513, Span(28215968,28227332), "wird"), Word(514, Span(28227332,28257320), "sein"))
  ), Vertex(
    List(Word(512, Span(28187126,28215968), "gebacken"), Word(513, Span(28215968,28227332), "wird"), Word(514, Span(28227332,28257320), "sein"))
  ), Vertex(
    List(Word(515, Span(28331541,28345741), "als"), Word(516, Span(28345741,28357560), "das"), Word(517, Span(28357560,28401704), "große"))
  ), Vertex(
    List(Word(516, Span(28345741,28357560), "das"), Word(517, Span(28357560,28401704), "große"))
  ), Vertex(
    List(Word(517, Span(28357560,28401704), "große"))
  ), Vertex(
    List(Word(518, Span(28444765,28460112), "vom"), Word(519, Span(28460112,28495127), "ganzen"))
  ), Vertex(
    List(Word(520, Span(28561171,28580179), "und"), Word(521, Span(28580179,28652679), "ausgebacken"))
  ), Vertex(
    List(Word(522, Span(28718550,28743422), "dann"))
  ), Vertex(
    List(Word(523, Span(28801984,28812436), "bin"), Word(524, Span(28812436,28823372), "ich"), Word(525, Span(28823372,28859887), "längst"))
  ), Vertex(
    List(Word(524, Span(28812436,28823372), "ich"), Word(525, Span(28823372,28859887), "längst"))
  ), Vertex(
    List(Word(525, Span(28823372,28859887), "längst"))
  ), Vertex(
    List(Word(526, Span(28904788,28931425), "wer"), Word(527, Span(28931425,28970806), "weiß"))
  ), Vertex(
    List(Word(527, Span(28931425,28970806), "weiß"))
  ), Vertex(
    List(Word(528, Span(28977499,29007620), "wo"))
  ), Vertex(
    List(Word(529, Span(29053405,29084495), "wer"))
  ), Vertex(
    List(Word(530, Span(29097025,29155942), "dann"))
  ), Vertex(
    List(Word(531, Span(29204197,29265937), "gegessen"))
  ), Vertex(
    List(Word(532, Span(29290944,29314009), "haben"), Word(533, Span(29314009,29353144), "wird"))
  ), Vertex(
    List(Word(534, Span(29383118,29403581), "vom"), Word(535, Span(29403581,29451032), "tellerchen"))
  ), Vertex(
    List(Word(536, Span(29497900,29557347), "und"))
  ), Vertex(
    List(Word(537, Span(29567987,29585230), "wer"), Word(538, Span(29585230,29654731), "dann"))
  ), Vertex(
    List(Word(539, Span(29677338,29692640), "noch"), Word(540, Span(29692640,29717689), "fragt"))
  ), Vertex(
    List(Word(540, Span(29692640,29717689), "fragt"))
  ), Vertex(
    List(Word(541, Span(29802378,29818695), "nach"), Word(542, Span(29818695,29829455), "den"), Word(543, Span(29829455,29907591), "zwergen"))
  ), Vertex(
    List(Word(543, Span(29829455,29907591), "zwergen"))
  ), Vertex(
    List(Word(544, Span(29922770,29944070), "dem"), Word(545, Span(29944070,29996990), "wittchen"))
  ), Vertex(
    List(Word(545, Span(29944070,29996990), "wittchen"))
  ), Vertex(
    List(Word(546, Span(30044611,30060178), "wer"), Word(547, Span(30060178,30077421), "noch"), Word(548, Span(30077421,30100210), "etwas"), Word(549, Span(30100210,30133516), "weiß"))
  ), Vertex(
    List(Word(548, Span(30077421,30100210), "etwas"), Word(549, Span(30100210,30133516), "weiß"))
  ), Vertex(
    List(Word(549, Span(30100210,30133516), "weiß"))
  ), Vertex(
    List(Word(550, Span(30172312,30189555), "wird"), Word(551, Span(30189555,30206372), "keine"), Word(552, Span(30206372,30231897), "spur"), Word(553, Span(30231897,30264047), "sich"))
  ), Vertex(
    List(Word(551, Span(30189555,30206372), "keine"), Word(552, Span(30206372,30231897), "spur"), Word(553, Span(30231897,30264047), "sich"))
  ), Vertex(
    List(Word(553, Span(30231897,30264047), "sich"))
  ), Vertex(
    List(Word(554, Span(30270706,30320520), "weit"), Word(555, Span(30320520,30339058), "und"), Word(556, Span(30339058,30358065), "breit"), Word(557, Span(30358065,30421320), "finden"))
  ), Vertex(
    List(Word(555, Span(30320520,30339058), "und"), Word(556, Span(30339058,30358065), "breit"), Word(557, Span(30358065,30421320), "finden"))
  ), Vertex(
    List(Word(558, Span(30442673,30464282), "vom"), Word(559, Span(30464282,30498057), "wolf"))
  ), Vertex(
    List(Word(560, Span(30543106,30553249), "wird"), Word(561, Span(30553249,30565703), "sich"), Word(562, Span(30565703,30614213), "zeigen"))
  ), Vertex(
    List(Word(562, Span(30565703,30614213), "zeigen"))
  ), Vertex(
    List(Word(563, Span(30622708,30671792), "nichts"))
  ), Vertex(
    List(Word(564, Span(30708399,30719335), "doch"), Word(565, Span(30719335,30727236), "in"), Word(566, Span(30727236,30738746), "aller"), Word(567, Span(30738746,30754923), "munde"), Word(568, Span(30754923,30770481), "ist"))
  ), Vertex(
    List(Word(569, Span(30785398,30891458), "eingelagertes"))
  ), Vertex(
    List(Word(570, Span(30896947,30953615), "zellgut"))
  ), Vertex(
    List(Word(571, Span(30999645,31015388), "von"), Word(572, Span(31015388,31079598), "gaumenseglern"))
  ), Vertex(
    List(Word(573, Span(31097128,31133819), "und"), Word(574, Span(31133819,31178801), "rhizomen"))
  ), Vertex(
    List(Word(574, Span(31133819,31178801), "rhizomen"))
  ), Vertex(
    List(Word(575, Span(31233581,31293072), "auserwählt"))
  ), Vertex(
    List(Word(576, Span(31337517,31351894), "vom"), Word(577, Span(31351894,31377692), "großen"), Word(578, Span(31377692,31427304), "professor"))
  ), Vertex(
    List(Word(578, Span(31377692,31427304), "professor"))
  ), Vertex(
    List(Word(579, Span(31470239,31490658), "gibt"), Word(580, Span(31490658,31518043), "heilung"))
  ), Vertex(
    List(Word(580, Span(31490658,31518043), "heilung"))
  ), Vertex(
    List(Word(581, Span(31526470,31533835), "die"), Word(582, Span(31533835,31570791), "braut"))
  ), Vertex(
    List(Word(582, Span(31533835,31570791), "braut"))
  ), Vertex(
    List(Word(583, Span(31597757,31608517), "dem"), Word(584, Span(31608517,31639255), "brennenden"), Word(585, Span(31639255,31670566), "dann"))
  ), Vertex(
    List(Word(584, Span(31608517,31639255), "brennenden"), Word(585, Span(31639255,31670566), "dann"))
  ), Vertex(
    List(Word(586, Span(31707276,31716449), "und"), Word(587, Span(31716449,31728180), "das"), Word(588, Span(31728180,31779821), "wirkliche"))
  ), Vertex(
    List(Word(587, Span(31716449,31728180), "das"), Word(588, Span(31728180,31779821), "wirkliche"))
  ), Vertex(
    List(Word(588, Span(31728180,31779821), "wirkliche"))
  ), Vertex(
    List(Word(589, Span(31805390,31842346), "begreifst"), Word(590, Span(31842346,31869026), "du"))
  ), Vertex(
    List(Word(590, Span(31842346,31869026), "du"))
  ), Vertex(
    List(Word(591, Span(31894638,31934504), "nicht"))
  ), Vertex(
    List(Word(592, Span(31974429,32012884), "bist"))
  ), Vertex(
    List(Word(593, Span(32033703,32048300), "eine"), Word(594, Span(32048300,32111500), "erscheinung"))
  ), Vertex(
    List(Word(595, Span(32118635,32149858), "sagt"), Word(596, Span(32149858,32177420), "er"))
  ), Vertex(
    List(Word(597, Span(32226204,32269466), "und"))
  ), Vertex(
    List(Word(598, Span(32275957,32288657), "wer"), Word(599, Span(32288657,32299109), "bist"), Word(600, Span(32299109,32323055), "du"))
  ), Vertex(
    List(Word(600, Span(32299109,32323055), "du"))
  ), Vertex(
    List(Word(601, Span(32372616,32384876), "über"), Word(602, Span(32384876,32404662), "dem"))
  ), Vertex(
    List(Word(603, Span(32406306,32457947), "haupt"))
  ), Vertex(
    List(Word(604, Span(32476660,32538929), "fragt"))
  ), Vertex(
    List(Word(605, Span(32573709,32605285), "mein"), Word(606, Span(32605285,32622308), "ich"), Word(607, Span(32622308,32650576), "mich"))
  ), Vertex(
    List(Word(606, Span(32605285,32622308), "ich"), Word(607, Span(32622308,32650576), "mich"))
  ), Vertex(
    List(Word(607, Span(32622308,32650576), "mich"))
  ), Vertex(
    List(Word(608, Span(32689156,32704900), "so"), Word(609, Span(32704900,32759769), "nebensächlich"))
  ), Vertex(
    List(Word(609, Span(32704900,32759769), "nebensächlich"))
  ), Vertex(
    List(Word(610, Span(32769816,32806154), "saugt"), Word(611, Span(32806154,32816726), "sich"), Word(612, Span(32816726,32848213), "an"))
  ), Vertex(
    List(Word(613, Span(32890548,32917493), "mein"), Word(614, Span(32917493,32959829), "bewusstsein"))
  ), Vertex(
    List(Word(614, Span(32917493,32959829), "bewusstsein"))
  ), Vertex(
    List(Word(615, Span(32993811,33019522), "liebt"), Word(616, Span(33019522,33031340), "mich"), Word(617, Span(33031340,33041792), "so"), Word(618, Span(33041792,33075925), "sehr"))
  ), Vertex(
    List(Word(619, Span(33146769,33164673), "mischt"), Word(620, Span(33164673,33185092), "sich"))
  ), Vertex(
    List(Word(620, Span(33164673,33185092), "sich"))
  ), Vertex(
    List(Word(621, Span(33187341,33207891), "immer"), Word(622, Span(33207891,33218343), "wo"), Word(623, Span(33218343,33263854), "ein"))
  ), Vertex(
    List(Word(623, Span(33218343,33263854), "ein"))
  ), Vertex(
    List(Word(624, Span(33324327,33346849), "und"))
  ), Vertex(
    List(Word(625, Span(33356565,33388052), "dort"))
  ), Vertex(
    List(Word(626, Span(33439668,33451487), "will"), Word(627, Span(33451487,33460571), "ich"), Word(628, Span(33460571,33486194), "es"))
  ), Vertex(
    List(Word(629, Span(33489402,33502897), "aber"), Word(630, Span(33502897,33526738), "nicht"))
  ), Vertex(
    List(Word(631, Span(33562227,33572105), "so"), Word(632, Span(33572105,33631758), "vergraben"))
  ), Vertex(
    List(Word(632, Span(33572105,33631758), "vergraben"))
  ), Vertex(
    List(Word(633, Span(33649454,33681823), "sehe"), Word(634, Span(33681823,33698713), "keine"), Word(635, Span(33698713,33744004), "aussicht"))
  ), Vertex(
    List(Word(635, Span(33698713,33744004), "aussicht"))
  ), Vertex(
    List(Word(636, Span(33771914,33784306), "auf"), Word(637, Span(33784306,33793832), "das"), Word(638, Span(33793832,33835903), "darüber"))
  ), Vertex(
    List(Word(637, Span(33784306,33793832), "das"), Word(638, Span(33793832,33835903), "darüber"))
  ), Vertex(
    List(Word(638, Span(33793832,33835903), "darüber"))
  ), Vertex(
    List(Word(639, Span(33884068,33923890), "hinweg"))
  ), Vertex(
    List(Word(640, Span(33984031,34005244), "muss"), Word(641, Span(34005244,34017062), "es"), Word(642, Span(34017062,34069144), "hervorholen"))
  ), Vertex(
    List(Word(641, Span(34005244,34017062), "es"), Word(642, Span(34017062,34069144), "hervorholen"))
  ), Vertex(
    List(Word(642, Span(34017062,34069144), "hervorholen"))
  ), Vertex(
    List(Word(643, Span(34070913,34114925), "immer"))
  ), Vertex(
    List(Word(644, Span(34142643,34172146), "muss"), Word(645, Span(34172146,34185331), "doch"), Word(646, Span(34185331,34205750), "dort"), Word(647, Span(34205750,34228139), "das"))
  ), Vertex(
    List(Word(645, Span(34172146,34185331), "doch"), Word(646, Span(34185331,34205750), "dort"), Word(647, Span(34205750,34228139), "das"))
  ), Vertex(
    List(Word(646, Span(34185331,34205750), "dort"), Word(647, Span(34205750,34228139), "das"))
  ), Vertex(
    List(Word(647, Span(34205750,34228139), "das"))
  ), Vertex(
    List(Word(648, Span(34229507,34267344), "sein"))
  ), Vertex(
    List(Word(649, Span(34325776,34334772), "was"), Word(650, Span(34334772,34349140), "sich"), Word(651, Span(34349140,34370765), "hier"))
  ), Vertex(
    List(Word(652, Span(34378964,34394002), "mir"), Word(653, Span(34394002,34410488), "nicht"), Word(654, Span(34410488,34463629), "findet"))
  ), Vertex(
    List(Word(654, Span(34410488,34463629), "findet"))
  ), Vertex(
    List(Word(655, Span(34554686,34602358), "zumindest"))
  ), Vertex(
    List(Word(656, Span(34608997,34638947), "suche"), Word(657, Span(34638947,34647427), "ich"), Word(658, Span(34647427,34670668), "schon"))
  ), Vertex(
    List(Word(659, Span(34708164,34730964), "viele"), Word(660, Span(34730964,34790543), "sommer"), Word(661, Span(34790543,35269090), "lang"))
  ), Vertex(
    List(Word(660, Span(34730964,34790543), "sommer"), Word(661, Span(34790543,35269090), "lang"))
  ), Vertex(
    List(Word(662, Span(35295725,35305295), "und"), Word(663, Span(35305295,35346881), "länger"))
  ), Vertex(
    List(Word(664, Span(35378553,35392753), "als"), Word(665, Span(35392753,35400617), "ich"), Word(666, Span(35400617,35443968), "dachte"))
  ), Vertex(
    List(Word(665, Span(35392753,35400617), "ich"), Word(666, Span(35400617,35443968), "dachte"))
  ), Vertex(
    List(Word(666, Span(35400617,35443968), "dachte"))
  ), Vertex(
    List(Word(667, Span(35521117,35535582), "seit"), Word(668, Span(35535582,35553972), "meiner"), Word(669, Span(35553972,35622120), "geburt"))
  ), Vertex(
    List(Word(668, Span(35535582,35553972), "meiner"), Word(669, Span(35553972,35622120), "geburt"))
  ), Vertex(
    List(Word(669, Span(35553972,35622120), "geburt"))
  ), Vertex(
    List(Word(670, Span(35645063,35673595), "scheint"), Word(671, Span(35673595,35702922), "mir"))
  ), Vertex(
    List(Word(672, Span(35785055,35802635), "wird"), Word(673, Span(35802635,35825937), "text"))
  ), Vertex(
    List(Word(673, Span(35802635,35825937), "text"))
  ), Vertex(
    List(Word(674, Span(35842818,35856533), "zum"), Word(675, Span(35856533,35885019), "körper"))
  ), Vertex(
    List(Word(675, Span(35856533,35885019), "körper"))
  ), Vertex(
    List(Word(676, Span(35961808,35973677), "und"), Word(677, Span(35973677,35996360), "geht"), Word(678, Span(35996360,36007438), "mir"), Word(679, Span(36007438,36045155), "davon"))
  ), Vertex(
    List(Word(677, Span(35973677,35996360), "geht"), Word(678, Span(35996360,36007438), "mir"), Word(679, Span(36007438,36045155), "davon"))
  ), Vertex(
    List(Word(678, Span(35996360,36007438), "mir"), Word(679, Span(36007438,36045155), "davon"))
  ), Vertex(
    List(Word(679, Span(36007438,36045155), "davon"))
  ), Vertex(
    List(Word(680, Span(36126119,36133768), "und"), Word(681, Span(36133768,36151176), "davon"), Word(682, Span(36151176,36188365), "aus"))
  ), Vertex(
    List(Word(682, Span(36151176,36188365), "aus"))
  ), Vertex(
    List(Word(683, Span(36218697,36227929), "und"), Word(684, Span(36227929,36254832), "überlässt"), Word(685, Span(36254832,36273822), "sich"))
  ), Vertex(
    List(Word(684, Span(36227929,36254832), "überlässt"), Word(685, Span(36254832,36273822), "sich"))
  ), Vertex(
    List(Word(686, Span(36351254,36363105), "und"), Word(687, Span(36363105,36382888), "gibt"), Word(688, Span(36382888,36410056), "sich"))
  ), Vertex(
    List(Word(687, Span(36363105,36382888), "gibt"), Word(688, Span(36382888,36410056), "sich"))
  ), Vertex(
    List(Word(688, Span(36382888,36410056), "sich"))
  ), Vertex(
    List(Word(689, Span(36429130,36454656), "her"))
  ), Vertex(
    List(Word(690, Span(36520173,36529471), "und"), Word(691, Span(36529471,36561416), "verbiegt"), Word(692, Span(36561416,36597578), "sich"))
  ), Vertex(
    List(Word(691, Span(36529471,36561416), "verbiegt"), Word(692, Span(36561416,36597578), "sich"))
  ), Vertex(
    List(Word(692, Span(36561416,36597578), "sich"))
  ), Vertex(
    List(Word(693, Span(36642061,36654460), "und"), Word(694, Span(36654460,36666676), "geht"), Word(695, Span(36666676,36678072), "eine"), Word(696, Span(36678072,36708339), "verbindung"), Word(697, Span(36708339,36727484), "ein"))
  ), Vertex(
    List(Word(694, Span(36654460,36666676), "geht"), Word(695, Span(36666676,36678072), "eine"), Word(696, Span(36678072,36708339), "verbindung"), Word(697, Span(36708339,36727484), "ein"))
  ), Vertex(
    List(Word(695, Span(36666676,36678072), "eine"), Word(696, Span(36678072,36708339), "verbindung"), Word(697, Span(36708339,36727484), "ein"))
  ), Vertex(
    List(Word(697, Span(36708339,36727484), "ein"))
  ), Vertex(
    List(Word(698, Span(36798715,36815125), "oder"), Word(699, Span(36815125,36828982), "auch"), Word(700, Span(36828982,36858575), "nicht"))
  ), Vertex(
    List(Word(699, Span(36815125,36828982), "auch"), Word(700, Span(36828982,36858575), "nicht"))
  ), Vertex(
    List(Word(700, Span(36828982,36858575), "nicht"))
  ), Vertex(
    List(Word(701, Span(36918814,36945981), "öffnet"), Word(702, Span(36945981,36961479), "sich"))
  ), Vertex(
    List(Word(703, Span(36965126,36979348), "ohne"), Word(704, Span(36979348,37034777), "schlussstrich"))
  ), Vertex(
    List(Word(704, Span(36979348,37034777), "schlussstrich"))
  ), Vertex(
    List(Word(705, Span(37103268,37123324), "bleibt"), Word(706, Span(37123324,37157967), "körper"))
  ), Vertex(
    List(Word(706, Span(37123324,37157967), "körper"))
  ), Vertex(
    List(Word(707, Span(37243484,37274298), "weiches"), Word(708, Span(37274298,37305476), "gebilde"))
  ), Vertex(
    List(Word(708, Span(37274298,37305476), "gebilde"))
  ), Vertex(
    List(Word(709, Span(37369626,37411745), "urform"))
  ), Vertex(
    List(Word(710, Span(37482556,37489850), "die"), Word(711, Span(37489850,37516470), "geht"), Word(712, Span(37516470,37523763), "mit"), Word(713, Span(37523763,37533609), "dem"), Word(714, Span(37533609,37587101), "wind"))
  ), Vertex(
    List(Word(711, Span(37489850,37516470), "geht"), Word(712, Span(37516470,37523763), "mit"), Word(713, Span(37523763,37533609), "dem"), Word(714, Span(37533609,37587101), "wind"))
  ), Vertex(
    List(Word(712, Span(37516470,37523763), "mit"), Word(713, Span(37523763,37533609), "dem"), Word(714, Span(37533609,37587101), "wind"))
  ), Vertex(
    List(Word(713, Span(37523763,37533609), "dem"), Word(714, Span(37533609,37587101), "wind"))
  ), Vertex(
    List(Word(715, Span(37632453,37663814), "verbiegt"), Word(716, Span(37663814,37673113), "sich"), Word(717, Span(37673113,37682229), "für"), Word(718, Span(37682229,37733282), "alles"))
  ), Vertex(
    List(Word(717, Span(37673113,37682229), "für"), Word(718, Span(37682229,37733282), "alles"))
  ), Vertex(
    List(Word(718, Span(37682229,37733282), "alles"))
  ), Vertex(
    List(Word(719, Span(37764337,37778559), "und"), Word(720, Span(37778559,37812108), "jedes"))
  ), Vertex(
    List(Word(721, Span(37870086,37915304), "verbiegt"))
  ), Vertex(
    List(Word(722, Span(37951356,37966307), "was"), Word(723, Span(37966307,37982717), "hand"), Word(724, Span(37982717,38007514), "anlegt"), Word(725, Span(38007514,38017725), "auf"), Word(726, Span(38017725,38038814), "ihm"))
  ), Vertex(
    List(Word(727, Span(38104386,38117513), "und"), Word(728, Span(38117513,38123713), "die"), Word(729, Span(38123713,38135017), "hand"))
  ), Vertex(
    List(Word(728, Span(38117513,38123713), "die"), Word(729, Span(38123713,38135017), "hand"))
  ), Vertex(
    List(Word(730, Span(38136294,38205580), "hat"))
  ), Vertex(
    List(Word(731, Span(38233620,38240184), "die"), Word(732, Span(38240184,38248571), "sich"), Word(733, Span(38248571,38270087), "seiner"), Word(734, Span(38270087,38306188), "bedient"))
  ), Vertex(
    List(Word(732, Span(38240184,38248571), "sich"), Word(733, Span(38248571,38270087), "seiner"), Word(734, Span(38270087,38306188), "bedient"))
  ), Vertex(
    List(Word(734, Span(38270087,38306188), "bedient"))
  ), Vertex(
    List(Word(735, Span(38552159,38571376), "sich"), Word(736, Span(38571376,38581305), "im"), Word(737, Span(38581305,38641519), "hingeben"))
  ), Vertex(
    List(Word(738, Span(38666241,38685885), "nicht"), Word(739, Span(38685885,38736641), "aufopfert"))
  ), Vertex(
    List(Word(739, Span(38685885,38736641), "aufopfert"))
  ), Vertex(
    List(Word(740, Span(38806222,38828002), "dafür"), Word(741, Span(38828002,38839105), "sich"), Word(742, Span(38839105,38858536), "nehmen"), Word(743, Span(38858536,38885867), "lässt"))
  ), Vertex(
    List(Word(744, Span(38903376,38912771), "als"), Word(745, Span(38912771,38923661), "dass"), Word(746, Span(38923661,38930707), "es"), Word(747, Span(38930707,38955903), "genommen"), Word(748, Span(38955903,38982167), "wird"))
  ), Vertex(
    List(Word(745, Span(38912771,38923661), "dass"), Word(746, Span(38923661,38930707), "es"), Word(747, Span(38930707,38955903), "genommen"), Word(748, Span(38955903,38982167), "wird"))
  ), Vertex(
    List(Word(747, Span(38930707,38955903), "genommen"), Word(748, Span(38955903,38982167), "wird"))
  ), Vertex(
    List(Word(749, Span(38997951,39004784), "und"), Word(750, Span(39004784,39040656), "aufgeht"))
  ), Vertex(
    List(Word(750, Span(39004784,39040656), "aufgeht"))
  ), Vertex(
    List(Word(751, Span(39044286,39076528), "diesem"))
  ), Vertex(
    List(Word(752, Span(39129035,39140992), "und"), Word(753, Span(39140992,39148893), "nicht"), Word(754, Span(39148893,39167896), "einmal"), Word(755, Span(39167896,39178573), "als"), Word(756, Span(39178573,39203128), "das"))
  ), Vertex(
    List(Word(753, Span(39140992,39148893), "nicht"), Word(754, Span(39148893,39167896), "einmal"), Word(755, Span(39167896,39178573), "als"), Word(756, Span(39178573,39203128), "das"))
  ), Vertex(
    List(Word(755, Span(39167896,39178573), "als"), Word(756, Span(39178573,39203128), "das"))
  ), Vertex(
    List(Word(756, Span(39178573,39203128), "das"))
  ), Vertex(
    List(Word(757, Span(39204409,39228751), "gesehen"), Word(758, Span(39228751,39262701), "wird"))
  ), Vertex(
    List(Word(759, Span(39290668,39300277), "was"), Word(760, Span(39300277,39308604), "es"), Word(761, Span(39308604,39328889), "innerlich"), Word(762, Span(39328889,39352804), "ist"))
  ), Vertex(
    List(Word(762, Span(39328889,39352804), "ist"))
  ), Vertex(
    List(Word(763, Span(39458469,39477259), "stellt"), Word(764, Span(39477259,39493914), "sich"), Word(765, Span(39493914,39521458), "immer"))
  ), Vertex(
    List(Word(764, Span(39477259,39493914), "sich"), Word(765, Span(39493914,39521458), "immer"))
  ), Vertex(
    List(Word(765, Span(39493914,39521458), "immer"))
  ), Vertex(
    List(Word(766, Span(39522953,39557971), "bloß"))
  ), Vertex(
    List(Word(767, Span(39626659,39646090), "stellt"), Word(768, Span(39646090,39660182), "sich"), Word(769, Span(39660182,39685592), "immer"))
  ), Vertex(
    List(Word(768, Span(39646090,39660182), "sich"), Word(769, Span(39660182,39685592), "immer"))
  ), Vertex(
    List(Word(769, Span(39660182,39685592), "immer"))
  ), Vertex(
    List(Word(770, Span(39689008,39713777), "dar"))
  ), Vertex(
    List(Word(771, Span(39760539,39771215), "und"), Word(772, Span(39771215,39787230), "stellt"), Word(773, Span(39787230,39808155), "sich"))
  ), Vertex(
    List(Word(774, Span(39828504,39864996), "ein"))
  ), Vertex(
    List(Word(775, Span(39898307,39918379), "zeigt"), Word(776, Span(39918379,39935247), "sich"), Word(777, Span(39935247,39971546), "selbst"))
  ), Vertex(
    List(Word(776, Span(39918379,39935247), "sich"), Word(777, Span(39935247,39971546), "selbst"))
  ), Vertex(
    List(Word(777, Span(39935247,39971546), "selbst"))
  ), Vertex(
    List(Word(778, Span(40023433,40043718), "als"), Word(779, Span(40043718,40070195), "das"))
  ), Vertex(
    List(Word(780, Span(40096245,40107348), "was"), Word(781, Span(40107348,40118665), "es"), Word(782, Span(40118665,40147491), "ist"))
  ), Vertex(
    List(Word(782, Span(40118665,40147491), "ist"))
  ), Vertex(
    List(Word(783, Span(40182807,40191348), "nur"), Word(784, Span(40191348,40197327), "in"), Word(785, Span(40197327,40205014), "der"), Word(786, Span(40205014,40237043), "textur"), Word(787, Span(40237043,40273769), "dort"))
  ), Vertex(
    List(Word(786, Span(40205014,40237043), "textur"), Word(787, Span(40237043,40273769), "dort"))
  ), Vertex(
    List(Word(787, Span(40237043,40273769), "dort"))
  ), Vertex(
    List(Word(788, Span(40277612,40293413), "kommt"), Word(789, Span(40293413,40306225), "es"), Word(790, Span(40306225,40315193), "zum"), Word(791, Span(40315193,40350211), "sein"))
  ), Vertex(
    List(Word(790, Span(40306225,40315193), "zum"), Word(791, Span(40315193,40350211), "sein"))
  ), Vertex(
    List(Word(792, Span(40425964,40439737), "als"), Word(793, Span(40439737,40470804), "vorschein"), Word(794, Span(40470804,40475336), "wie"), Word(795, Span(40475336,40484731), "zur"), Word(796, Span(40484731,40505443), "schau"), Word(797, Span(40505443,40550284), "gestellt"))
  ), Vertex(
    List(Word(794, Span(40470804,40475336), "wie"), Word(795, Span(40475336,40484731), "zur"), Word(796, Span(40484731,40505443), "schau"), Word(797, Span(40505443,40550284), "gestellt"))
  ), Vertex(
    List(Word(795, Span(40475336,40484731), "zur"), Word(796, Span(40484731,40505443), "schau"), Word(797, Span(40505443,40550284), "gestellt"))
  ), Vertex(
    List(Word(797, Span(40505443,40550284), "gestellt"))
  ), Vertex(
    List(Word(798, Span(40607321,40617571), "aus"), Word(799, Span(40617571,40662197), "verlautbarten"), Word(800, Span(40662197,40693585), "zeichen"))
  ), Vertex(
    List(Word(799, Span(40617571,40662197), "verlautbarten"), Word(800, Span(40662197,40693585), "zeichen"))
  ), Vertex(
    List(Word(800, Span(40662197,40693585), "zeichen"))
  ), Vertex(
    List(Word(801, Span(40767040,40777716), "wo"), Word(802, Span(40777716,40787324), "da"), Word(803, Span(40787324,40808250), "kommt"), Word(804, Span(40808250,40853622), "etwas"))
  ), Vertex(
    List(Word(802, Span(40777716,40787324), "da"), Word(803, Span(40787324,40808250), "kommt"), Word(804, Span(40808250,40853622), "etwas"))
  ), Vertex(
    List(Word(803, Span(40787324,40808250), "kommt"), Word(804, Span(40808250,40853622), "etwas"))
  ), Vertex(
    List(Word(805, Span(40871760,40889482), "zum"), Word(806, Span(40889482,40927276), "punkt"))
  ), Vertex(
    List(Word(806, Span(40889482,40927276), "punkt"))
  ), Vertex(
    List(Word(807, Span(41166113,41187893), "oder"), Word(808, Span(41187893,41225473), "etwas"))
  ), Vertex(
    List(Word(808, Span(41187893,41225473), "etwas"))
  ), Vertex(
    List(Word(809, Span(41245972,41280563), "sich"), Word(810, Span(41280563,41325403), "windet"))
  ), Vertex(
    List(Word(810, Span(41280563,41325403), "windet"))
  ), Vertex(
    List(Word(811, Span(41386498,41398456), "und"), Word(812, Span(41398456,41407424), "im"), Word(813, Span(41407424,41428349), "wort"), Word(814, Span(41428349,41464221), "drinnen"))
  ), Vertex(
    List(Word(812, Span(41398456,41407424), "im"), Word(813, Span(41407424,41428349), "wort"), Word(814, Span(41428349,41464221), "drinnen"))
  ), Vertex(
    List(Word(813, Span(41407424,41428349), "wort"), Word(814, Span(41428349,41464221), "drinnen"))
  ), Vertex(
    List(Word(814, Span(41428349,41464221), "drinnen"))
  ), Vertex(
    List(Word(815, Span(41491125,41514613), "wächst"), Word(816, Span(41514613,41524435), "es"), Word(817, Span(41524435,41548777), "sich"))
  ), Vertex(
    List(Word(817, Span(41524435,41548777), "sich"))
  ), Vertex(
    List(Word(818, Span(41566286,41606856), "aus"))
  ))

  def main(args: Array[String]): Unit = {
    run()
  }

  def mkWordsString(v: Vertex): String =
    quote(v.words.map(_.text).mkString(s"${v.index}: ", " ", ""))

  def run(): Unit = {
    val selection: List[Vertex] = vertices.take(128)
    val numComb   = selection.combinations(2).size
    println(s"Number of combinations: $numComb")

    val fileIn = file("/data/projects/Schwaermen/audio_work/Gertrude_DaDrehtSichDerBaum_slow_170605.aif")

    type SimEdge = Edge[Vertex]

    import scala.concurrent.ExecutionContext.Implicits.global

    val t: Future[List[SimEdge]] = Future {
      val af = AudioFile.openRead(fileIn)

      def copy(span: Span, target: File): Unit = {
        val len = span.length.toInt
        val b   = af.buffer(len)
        af.seek(span.start)
        af.read(b)
        val ch0 = b(0)
        var ch = 0
        while (ch < af.numChannels) {
          val chI = b(ch)
          var i = 0
          while (i < len) {
            ch0(i) += chI(i)
            i += 1
          }
          ch += 1
        }
        val afOut = AudioFile.openWrite(target, AudioFileSpec(numChannels = 1, sampleRate = af.sampleRate))
        try {
          afOut.write(Array(ch0))
        } finally {
          afOut.close()
        }
      }

      def run(): List[SimEdge] = try {
        def loop(rem: List[Vertex], res: List[SimEdge], mapTemp: Map[Vertex, File]): List[SimEdge] =
          rem match {
            case v1 :: tail if tail.nonEmpty =>
              var map = mapTemp

              def getTemp(v: Vertex): File =
                map.getOrElse(v, {
                  val f = File.createTemp()
                  map += v -> f
                  copy(v.span, f)
                  f
                })

              val fileA = getTemp(v1)
              // println(fileA)
              val edge = tail.map { v2 =>
                val fileB = getTemp(v2)
                // println(fileB)
                val name = s"${v1.words.head.index}-${v2.words.head.index}"
                val (g, fut) = mkGraph(fileA = fileA, fileB = fileB, name = name)
                val c = Control()
                // println(s"Running $name")
                c.run(g)
//                Await.result(c.status, Duration.Inf)
                val sim = Await.result(fut, Duration.Inf)
                println(f"${mkWordsString(v1)} -- ${mkWordsString(v2)} : $sim%g")
                Edge(v1, v2, sim): SimEdge
              }
              loop(tail, edge ::: res, map)

            case _ => res
          }

        val res = loop(selection, Nil, Map.empty)
        println("Done.")
        res

      } finally {
        af.cleanUp()
      }

      run()
    }

    val busy = new Thread {
      override def run(): Unit = synchronized(wait())
      start()
    }

    def release(): Unit = busy.synchronized(busy.notify())

    t.onComplete {
      case Success(edges) =>
        implicit val ord: Ordering[Vertex] = Ordering.by(_.words.head.index)
        val mst = MSTKruskal[Vertex, SimEdge](edges)

        def vName(v: Vertex): String = s"v${v.index}"

        val nodesSx = selection.map { v =>
          val label = mkWordsString(v)
          s"""${vName(v)} [label=$label]"""
        }
        val edgesSx = mst.map { e =>
          s"""${vName(e.start)} -- ${vName(e.end)}"""
        }
        val nodesS = nodesSx.mkString("  ", "\n  ", "")
        val edgesS = edgesSx.mkString("  ", "\n  ", "")
        val viz =
          s"""graph {
             |$nodesS
             |$edgesS
             |}
             |""".stripMargin

        println(viz)
        release()

      case Failure(ex) => ex.printStackTrace()
        release()
    }
  }

  def escapedChar(ch: Char): String = ch match {
    case '\b' => "\\b"
    case '\t' => "\\t"
    case '\n' => "\\n"
    case '\f' => "\\f"
    case '\r' => "\\r"
    case '"'  => "\\\""
    case '\'' => "\\\'"
    case '\\' => "\\\\"
    case _    => if (ch.isControl) "\\0" + Integer.toOctalString(ch.toInt) else String.valueOf(ch)
  }

  /** Escapes characters such as newlines and quotation marks in a string. */
  def escape(s: String): String = s.flatMap(escapedChar)
  /** Escapes characters such as newlines in a string, and adds quotation marks around it.
    * That is, formats the string as a string literal in a source code.
    */
  def quote (s: String): String = "\"" + escape(s) + "\""

  def mkGraph(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.numbers.Implicits._
      import de.sciss.fscape._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA = AudioFileIn(fileA, numChannels = 1)
      val chunkB = AudioFileIn(fileB, numChannels = 1)

      def normalize(in: GE, len: GE): GE = {
        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        inB / (mx / len).sqrt
      }

      val mfccA = chunkA
      val mfccB = chunkB
      val lenA2 = lenA
      val lenB2 = lenB

      val convLen0= lenA2 + lenB2 - 1
      val convLen = convLen0.toInt.nextPowerOfTwo

      val mfccAN  = normalize(mfccA, lenA2)
      val mfccBN  = normalize(mfccB, lenB2)
      // val lenMin  = lenA2.min(lenB2)

      val chunkAP = ResizeWindow(mfccAN, size = lenA2, stop = convLen - lenA2)
      val chunkBP = ResizeWindow(mfccBN, size = lenB2, stop = convLen - lenB2)
      val chunkBR = ReverseWindow(in = chunkBP, size = convLen)
      val fftA    = Real1FFT(in = chunkAP, size = convLen, mode = 1)
      val fftB    = Real1FFT(in = chunkBR, size = convLen, mode = 1)
      val prod    = fftA.complex * fftB
      val corr    = Real1IFFT(in = prod, size = convLen, mode = 1)
      val max     = RunningMax(corr).last //  / /* (2 * lenMin) */ (lenA2 + lenB2) * convLen
      //      max.poll(0, s"similarity ($name)")
      Fulfill(max, p)
    }
    (g, p.future)
  }

  def mkGraphOLD(fileA: File, fileB: File, name: String): (Graph, Future[Double]) = {
    val p = Promise[Double]()
    val g = Graph {
      import de.sciss.numbers.Implicits._
      import de.sciss.fscape._
      import graph._

      val specA   = AudioFile.readSpec(fileA)
      val specB   = AudioFile.readSpec(fileB)
      val lenA    = specA.numFrames
      val lenB    = specB.numFrames

      val chunkA = AudioFileIn(fileA, numChannels = 1)
      val chunkB = AudioFileIn(fileB, numChannels = 1)

      def normalize(in: GE, len: GE): GE = {
        val inB = in.elastic(len/ControlBlockSize())
        val mx  = RunningSum(in.squared).last
        inB / (mx / len).sqrt
      }

      val fsz       = 1024
      val winStep   = fsz/2
      val melBands  = 42
      val numMFCC   = 42 // 13
      require(numMFCC == melBands) // we dropped the DCT

      def mfcc(in: GE): GE = {
        val lap  = Sliding(in, fsz, winStep) * GenWindow(fsz, GenWindow.Hann)
        val fft  = Real1FFT(lap, fsz, mode = 1)
        val mag  = fft.complex.mag.max(-80)
        val mel  = MelFilter(mag, winStep, bands = melBands)
        DCT_II(mel.log, melBands, numMFCC, zero = 0)
      }

      def ceilL(a: Long, b: Int): Long = {
        val c = a + b - 1
        c - (c % b)
      }

      val mfccA   = mfcc(chunkA)
      val mfccB   = mfcc(chunkB)
      val numWinA = ceilL(lenA, winStep)
      val numWinB = ceilL(lenB, winStep)
      val lenA2   = numWinA * numMFCC
      val lenB2   = numWinB * numMFCC

      val convLen0= lenA2 + lenB2 - 1
      val convLen = convLen0.toInt.nextPowerOfTwo

      val mfccAN  = normalize(mfccA, lenA2)
      val mfccBN  = normalize(mfccB, lenB2)
//      val lenMin  = lenA2.min(lenB2)

      val chunkAP = ResizeWindow(mfccAN, size = lenA2, stop = convLen - lenA2)
      val chunkBP = ResizeWindow(mfccBN, size = lenB2, stop = convLen - lenB2)
      val chunkBR = ReverseWindow(in = chunkBP, size = convLen)
      val fftA    = Real1FFT(in = chunkAP, size = convLen, mode = 1)
      val fftB    = Real1FFT(in = chunkBR, size = convLen, mode = 1)
      val prod    = fftA.complex * fftB
      val corr    = Real1IFFT(in = prod, size = convLen, mode = 1)
      val max     = RunningMax(corr).last //  / /* (2 * lenMin) */ (lenA2 + lenB2) * convLen
//      max.poll(0, s"similarity ($name)")
      Fulfill(max, p)
    }
    (g, p.future)
  }
}