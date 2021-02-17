lazy val scala212 = "2.12.12"
lazy val scala213 = "2.13.4"
lazy val supportedScalaVersions = List(scala212, scala213)

lazy val utilsVersion         = "0.1.73"

// Dependency versions
lazy val catsVersion           = "2.3.0"
lazy val catsEffectVersion     = "2.3.3"
lazy val circeVersion          = "0.14.0-M1"
lazy val fs2Version            = "2.4.4"
lazy val http4sVersion         = "0.21.3"
lazy val jenaVersion           = "3.16.0"
lazy val logbackVersion        = "1.2.3"
lazy val loggingVersion        = "3.9.2"
lazy val rdf4jVersion          = "3.4.2"
lazy val scalacheckVersion     = "1.14.0"
lazy val scalacticVersion      = "3.2.0"
lazy val scalaTestVersion      = "3.2.0"
//lazy val scalatagsVersion      = "0.6.7"
lazy val scallopVersion        = "3.3.1"
lazy val typesafeConfigVersion = "1.4.0"

// Compiler plugin dependency versions
lazy val simulacrumVersion       = "1.0.0"
lazy val scalaMacrosVersion      = "2.1.1"
lazy val scalaCollCompatVersion  = "2.2.0"

// Dependency modules

lazy val utils             = "es.weso"                    %% "utils"               % utilsVersion

lazy val catsCore          = "org.typelevel"              %% "cats-core"           % catsVersion
lazy val catsKernel        = "org.typelevel"              %% "cats-kernel"         % catsVersion
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
lazy val scalaCollCompat   = "org.scala-lang.modules"     %% "scala-collection-compat" % scalaCollCompatVersion
lazy val scalaTest         = "org.scalatest"              %% "scalatest"           % scalaTestVersion
lazy val typesafeConfig    = "com.typesafe"               % "config"               % typesafeConfigVersion

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }


lazy val srdfMain = project
  .in(file("."))
  .enablePlugins(ScalaUnidocPlugin, SiteScaladocPlugin, AsciidoctorPlugin)
  .settings(
    commonSettings, 
    // packagingSettings, 
    publishSettings
  )
  .aggregate(srdfJena, srdf4j, srdf)
  .settings(
    siteSubdirName in ScalaUnidoc := "api/latest",
    addMappingsToSiteDir(mappings in (ScalaUnidoc, packageDoc), siteSubdirName in ScalaUnidoc),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := inAnyProject -- inProjects(noDocProjects: _*),
    libraryDependencies ++= Seq(
      logbackClassic,
      scalaLogging,
      scallop
    ),
    cancelable in Global      := true,
    fork                      := true,
    parallelExecution in Test := true,
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
      // catsMacros,
      catsEffect,
      circeCore,
      circeGeneric,
      circeParser,
      fs2Core,
      utils,
      scalaLogging,
      scalaCollCompat
    )
    )
    
  lazy val srdfJena = project
  .in(file("modules/srdfJena"))
  .dependsOn(srdf)
  .settings(commonSettings, publishSettings)
  .settings(
    parallelExecution in Test := true,
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
      // catsMacros,
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
    parallelExecution in Test := false,
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      logbackClassic % Test,
      scalaLogging,
      utils,
      rdf4j_runtime,
      catsCore,
      catsKernel,
      // catsMacros,
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
