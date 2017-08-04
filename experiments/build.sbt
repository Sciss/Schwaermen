lazy val webName          = "Schwaermen"
lazy val baseName         = s"$webName-Experiments"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "An art project - experiments",
  homepage            := Some(url(s"https://github.com/Sciss/$webName")),
  scalaVersion        := "2.12.3",
  licenses            := Seq(gpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"          %% "fileutil"           % "1.1.2",
    "de.sciss"          %% "numbers"            % "0.1.3",
    "de.sciss"          %% "kollflitz"          % "0.2.1",
    "com.github.scopt"  %% "scopt"              % "3.6.0",
    "de.sciss"          %  "shapeinterpolator"  % "0.1.0",
    "de.sciss"          %% "swingplus"          % "0.2.4",
    "de.sciss"          %% "span"               % "1.3.2",
    "de.sciss"          %% "fscape"             % "2.8.0",
    "de.sciss"          %  "prefuse-core"       % "1.0.1",
    "de.sciss"          %% "desktop"            % "0.7.5",
    "de.sciss"          %% "soundprocesses"     % "3.13.0"
  )
)

lazy val gpl2 = "GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)

