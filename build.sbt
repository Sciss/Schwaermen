import com.typesafe.sbt.packager.linux.LinuxPackageMapping

lazy val baseName         = "Schwaermen"
lazy val baseNameL        = baseName.toLowerCase
lazy val projectVersion   = "0.1.5-SNAPSHOT"

lazy val baseDescr        = "An art project"

lazy val commonSettings = Seq(
  version             := projectVersion,
  organization        := "de.sciss",
  homepage            := Some(url(s"https://github.com/Sciss/$baseName")),
  scalaVersion        := "2.12.3",
  licenses            := Seq(gpl2),
  scalacOptions      ++= Seq("-deprecation", "-unchecked", "-feature", "-encoding", "utf8", "-Xfuture", "-Xlint:-stars-align,_")
)

lazy val fileUtilVersion        = "1.1.3"
lazy val pi4jVersion            = "1.1"
lazy val numbersVersion         = "0.1.3"
lazy val kollFlitzVersion       = "0.2.1"
lazy val scalaOSCVersion        = "1.1.5"
lazy val scoptVersion           = "3.6.0"
lazy val soundProcessesVersion  = "3.13.0"
lazy val swingPlusVersion       = "0.2.4"

lazy val root = Project(id = baseNameL, base = file("."))
  .aggregate(common, sound, control)
  .settings(commonSettings)

lazy val common = Project(id = s"$baseName-common", base = file("common"))
  .settings(commonSettings)
  .settings(
    description := s"$baseDescr - common structure",
    libraryDependencies ++= Seq(
      "de.sciss"          %% "fileutil"   % fileUtilVersion,
      "de.sciss"          %% "numbers"    % numbersVersion,
      "de.sciss"          %% "scalaosc"   % scalaOSCVersion,
      "de.sciss"          %% "kollflitz"  % kollFlitzVersion,
      "com.github.scopt"  %% "scopt"      % scoptVersion
    )
  )

lazy val soundName  = s"$baseName-Sound"
lazy val soundNameL = soundName.toLowerCase

lazy val sound = Project(id = soundNameL, base = file("sound"))
  .dependsOn(common)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging, DebianPlugin)
  .settings(commonSettings)
  .settings(
    description := s"$baseDescr - rpi sound",
    libraryDependencies ++= Seq(
      "de.sciss" %% "soundprocesses" % soundProcessesVersion,
      "com.pi4j" %  "pi4j-core"      % pi4jVersion
    ),
    buildInfoPackage := "de.sciss.schwaermen.sound",
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoOptions += BuildInfoOption.BuildTime
  )
  .settings(soundDebianSettings)

lazy val videoName  = s"$baseName-Video"
lazy val videoNameL = videoName.toLowerCase

lazy val video = Project(id = videoNameL, base = file("video"))
  .dependsOn(common)
  .enablePlugins(BuildInfoPlugin)
  .enablePlugins(JavaAppPackaging, DebianPlugin)
  .settings(commonSettings)
  .settings(
    description := s"$baseDescr - rpi video",
    libraryDependencies ++= Seq(
      "de.sciss" %% "swingplus" % swingPlusVersion
    ),
    buildInfoPackage := "de.sciss.schwaermen.video",
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoOptions += BuildInfoOption.BuildTime
  )
  .settings(videoDebianSettings)

lazy val control = Project(id = s"$baseNameL-control", base = file("control"))
  .dependsOn(common)
  .enablePlugins(BuildInfoPlugin)
  .settings(commonSettings)
  .settings(
    description := s"$baseDescr - laptop control",
    libraryDependencies ++= Seq(
      "de.sciss" %% "swingplus"  % swingPlusVersion,
      "de.sciss" %% "desktop"    % "0.8.0",
      "de.sciss" %% "model"      % "0.3.4"
    ),
    buildInfoPackage := "de.sciss.schwaermen.control",
    buildInfoKeys := Seq(name, organization, version, scalaVersion, description,
      BuildInfoKey.map(homepage) { case (k, opt)           => k -> opt.get },
      BuildInfoKey.map(licenses) { case (_, Seq((lic, _))) => "license" -> lic }
    ),
    buildInfoOptions += BuildInfoOption.BuildTime
  )

lazy val gpl2 = "GPL v2+" -> url("http://www.gnu.org/licenses/gpl-2.0.txt")

// ---- debian package ----

lazy val maintainerHH = "Hanns Holger Rutz <contact@sciss.de>"

lazy val soundDebianSettings = useNativeZip ++ Seq[Def.Setting[_]](
  executableScriptName /* in Universal */ := soundNameL,
  scriptClasspath /* in Universal */ := Seq("*"),
  name        in Debian := soundName,
  packageName in Debian := soundNameL,
  name        in Linux  := soundName,
  packageName in Linux  := soundNameL,
  mainClass   in Debian := Some("de.sciss.schwaermen.sound.Main"),
  maintainer  in Debian := maintainerHH,
  debianPackageDependencies in Debian += "java8-runtime",
  packageSummary in Debian := description.value,
  packageDescription in Debian :=
    """Software for a sound installation - Schwaermen+Vernetzen.
      |""".stripMargin
) ++ commonDebianSettings

lazy val videoDebianSettings = useNativeZip ++ Seq[Def.Setting[_]](
  executableScriptName /* in Universal */ := videoNameL,
  scriptClasspath /* in Universal */ := Seq("*"),
  name        in Debian := videoName,
  packageName in Debian := videoNameL,
  name        in Linux  := videoName,
  packageName in Linux  := videoNameL,
  mainClass   in Debian := Some("de.sciss.schwaermen.video.Main"),
  maintainer  in Debian := maintainerHH,
  debianPackageDependencies in Debian += "java8-runtime",
  packageSummary in Debian := description.value,
  packageDescription in Debian :=
    """Software for a video installation - Schwaermen+Vernetzen.
      |""".stripMargin
) ++ commonDebianSettings

lazy val commonDebianSettings = Seq(
  // include all files in src/debian in the installed base directory
  linuxPackageMappings in Debian ++= {
    val n     = (name            in Debian).value.toLowerCase
    val dir   = (sourceDirectory in Debian).value / "debian"
    val f1    = (dir * "*").filter(_.isFile).get  // direct child files inside `debian` folder
    val f2    = ((dir / "doc") * "*").get
    //
    def readOnly(in: LinuxPackageMapping) =
      in.withUser ("root")
        .withGroup("root")
        .withPerms("0644")  // http://help.unc.edu/help/how-to-use-unix-and-linux-file-permissions/
    //
    val aux   = f1.map { fIn => packageMapping(fIn -> s"/usr/share/$n/${fIn.name}") }
    val doc   = f2.map { fIn => packageMapping(fIn -> s"/usr/share/doc/$n/${fIn.name}") }
    (aux ++ doc).map(readOnly)
  }
)

