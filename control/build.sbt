lazy val webName          = "Schwaermen"
lazy val baseName         = s"$webName-Control"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.0-SNAPSHOT"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  description         := "An art project - control",
  homepage            := Some(url(s"https://github.com/Sciss/$webName")),
  scalaVersion        := "2.12.3",
  licenses            := Seq(gpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint"),
  libraryDependencies ++= Seq(
    "de.sciss"          %% "fileutil"   % "1.1.2",
    "de.sciss"          %% "scalaosc"   % "1.1.5",
    "de.sciss"          %% "swingplus"  % "0.2.4",
    "de.sciss"          %% "model"      % "0.3.4",
    "com.github.scopt"  %% "scopt"      % "3.6.0"
  )
)

lazy val gpl2 = "GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")

lazy val root = Project(id = baseNameL, base = file("."))
  .settings(commonSettings)
