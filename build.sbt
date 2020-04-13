lazy val scala212 = "2.12.11"
lazy val scala213 = "2.13.1"
lazy val supportedScalaVersions = List(scala212, scala213)

lazy val utilsVersion         = "0.1.67"

// Dependency versions
lazy val catsVersion           = "2.1.1"
lazy val catsEffectVersion     = "2.1.2"
lazy val circeVersion          = "0.12.3"
lazy val fs2Version            = "2.2.1"
lazy val http4sVersion         = "0.21.3"
lazy val jenaVersion           = "3.13.1"
lazy val logbackVersion        = "1.2.3"
lazy val loggingVersion        = "3.9.2"
lazy val rdf4jVersion          = "3.0.0"
lazy val scalacheckVersion     = "1.14.0"
lazy val scalacticVersion      = "3.1.1"
lazy val scalaTestVersion      = "3.1.1"
lazy val scalatagsVersion      = "0.6.7"
lazy val scallopVersion        = "3.3.1"
lazy val typesafeConfigVersion = "1.4.0"

// Compiler plugin dependency versions
lazy val simulacrumVersion    = "1.0.0"
// lazy val kindProjectorVersion = "0.9.5"
lazy val scalaMacrosVersion   = "2.1.1"

// Dependency modules

lazy val utils             = "es.weso"                    %% "utils"              % utilsVersion

lazy val catsCore          = "org.typelevel"              %% "cats-core"           % catsVersion
lazy val catsKernel        = "org.typelevel"              %% "cats-kernel"         % catsVersion
lazy val catsMacros        = "org.typelevel"              %% "cats-macros"         % catsVersion
lazy val catsEffect        = "org.typelevel"              %% "cats-effect"         % catsEffectVersion
lazy val circeCore         = "io.circe"                   %% "circe-core"          % circeVersion
lazy val circeGeneric      = "io.circe"                   %% "circe-generic"       % circeVersion
lazy val circeParser       = "io.circe"                   %% "circe-parser"        % circeVersion
lazy val fs2Core           = "co.fs2"                     %% "fs2-core"            % fs2Version
lazy val http4sBlazeClient = "org.http4s"                 %% "http4s-blaze-client" % http4sVersion
lazy val jenaArq           = "org.apache.jena"            % "jena-arq"             % jenaVersion
lazy val jenaFuseki        = "org.apache.jena"            % "jena-fuseki-main"     % jenaVersion
lazy val logbackClassic    = "ch.qos.logback"             % "logback-classic"      % logbackVersion
lazy val rdf4j_runtime     = "org.eclipse.rdf4j"          % "rdf4j-runtime"        % rdf4jVersion
lazy val scalaLogging      = "com.typesafe.scala-logging" %% "scala-logging"       % loggingVersion
lazy val scallop           = "org.rogach"                 %% "scallop"             % scallopVersion
lazy val scalactic         = "org.scalactic"              %% "scalactic"           % scalacticVersion
lazy val scalacheck        = "org.scalacheck"             %% "scalacheck"          % scalacheckVersion
lazy val scalaTest         = "org.scalatest"              %% "scalatest"           % scalaTestVersion
lazy val scalatags         = "com.lihaoyi"                %% "scalatags"           % scalatagsVersion
lazy val typesafeConfig    = "com.typesafe"               % "config"               % typesafeConfigVersion

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }


lazy val srdfMain = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin, SbtNativePackager, WindowsPlugin, JavaAppPackaging, LauncherJarPlugin)
  .settings(commonSettings, packagingSettings, publishSettings, ghPagesSettings, wixSettings)
  .aggregate(srdfJena, srdf4j, srdf)
  .settings(
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(noDocProjects: _*),
    libraryDependencies ++= Seq(
      logbackClassic,
      scalaLogging,
      scallop
    ),
    cancelable in Global      := true,
    fork                      := true,
    parallelExecution in Test := false,
    crossScalaVersions := Nil,
    publish / skip := true,
    ThisBuild / turbo := true
  )

lazy val srdf = project
  .in(file("modules/srdf"))
  .settings(commonSettings, publishSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      catsCore,
      catsKernel,
      catsMacros,
      catsEffect,
      circeCore,
      circeGeneric,
      circeParser,
      fs2Core,
      utils,
      scalaLogging
    )
    )
    
  lazy val srdfJena = project
  .in(file("modules/srdfJena"))
  .dependsOn(srdf)
  .settings(commonSettings, publishSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      logbackClassic % Test,
      scalaLogging,
      jenaFuseki % Test,
      typesafeConfig % Test,
      utils,
      jenaArq,
      catsCore,
      catsKernel,
      catsMacros,
      catsEffect,
      fs2Core,
      http4sBlazeClient
    )
  )

lazy val srdf4j = project
  .in(file("modules/srdf4j"))
  .dependsOn(srdf)
  .settings(commonSettings, publishSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      logbackClassic % Test,
      scalaLogging,
      utils,
      rdf4j_runtime,
      catsCore,
      catsKernel,
      catsMacros,
      catsEffect,
      fs2Core
    )
  )

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noDocProjects = Seq[ProjectReference](
)

lazy val noPublishSettings = Seq(
//  publish := (),
//  publishLocal := (),
  publishArtifact := false
)

lazy val sharedDependencies = Seq(
  libraryDependencies ++= Seq(
    scalactic,
    scalaTest % Test
  )
)

lazy val packagingSettings = Seq(
  mainClass in Compile        := Some("es.weso.shaclex.Main"),
  mainClass in assembly       := Some("es.weso.shaclex.Main"),
  test in assembly            := {},
  assemblyJarName in assembly := "shaclex.jar",
  packageSummary in Linux     := name.value,
  packageSummary in Windows   := name.value,
  packageDescription          := name.value
)

lazy val wixSettings = Seq(
  wixProductId        := "39b564d5-d381-4282-ada9-87244c76e14b",
  wixProductUpgradeId := "6a710435-9af4-4adb-a597-98d3dd0bade1"
// The same numbers as in the docs?
// wixProductId := "ce07be71-510d-414a-92d4-dff47631848a",
// wixProductUpgradeId := "4552fb0e-e257-4dbd-9ecb-dba9dbacf424"
)

lazy val compilationSettings = Seq(
  scalaVersion := scala213,
  // format: off
  javacOptions ++= Seq("-source", "1.8", "-target", "1.8"),
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-explaintypes",                     // Explain type errors in more detail.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.  "-encoding", "UTF-8",
    "-language:_",
    "-target:jvm-1.8",
    "-unchecked",                        // Enable additional warnings where generated code depends on assumptions.
    "-Xlint",
    "-Yrangepos",
    "-Ywarn-dead-code",                  // Warn when dead code is identified.
    "-Ywarn-extra-implicit",             // Warn when more than one implicit parameter section is defined.
  )
  // format: on
)

lazy val ghPagesSettings = Seq(
  git.remoteRepo := "git@github.com:weso/srdf.git"
)

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  organization := "es.weso",
  resolvers ++= Seq(
    Resolver.bintrayRepo("labra", "maven"),
    Resolver.bintrayRepo("weso", "weso-releases"),
    Resolver.sonatypeRepo("snapshots")
  ), 
  coverageHighlighting := priorTo2_13(scalaVersion.value)
 )

lazy val publishSettings = Seq(
  maintainer      := "Jose Emilio Labra Gayo <labra@uniovi.es>",
  homepage        := Some(url("https://github.com/weso/srdf")),
  licenses        := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo         := Some(ScmInfo(url("https://github.com/weso/srdf"), "scm:git:git@github.com:weso/srdf.git")),
  autoAPIMappings := true,
  pomExtra        := <developers>
                       <developer>
                         <id>labra</id>
                         <name>Jose Emilio Labra Gayo</name>
                         <url>https://github.com/labra/</url>
                       </developer>
                     </developers>,
  scalacOptions in doc ++= Seq(
    "-diagrams-debug",
    "-doc-source-url",
    scmInfo.value.get.browseUrl + "/tree/master€{FILE_PATH}.scala",
    "-sourcepath",
    baseDirectory.in(LocalRootProject).value.getAbsolutePath,
    "-diagrams",
  ),
  publishMavenStyle              := true,
  bintrayRepository in bintray   := "weso-releases",
  bintrayOrganization in bintray := Some("weso")
)
