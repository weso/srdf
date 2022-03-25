lazy val scala212 = "2.12.15"
lazy val scala213 = "2.13.8"
lazy val scala3   = "3.1.0"
lazy val supportedScalaVersions = List(
  scala3,
  scala213,
  scala212
)

val Java11 = JavaSpec.temurin("11") // "adopt@1.11"
// val Java8 = JavaSpec.temurin("8") // "adopt@1.8"

lazy val utilsVersion = "0.2.4"

// Dependency versions
lazy val catsVersion        = "2.7.0"
lazy val catsEffectVersion  = "3.3.9"
lazy val circeVersion       = "0.14.1"
lazy val declineVersion     = "2.2.0"
lazy val fs2Version         = "3.2.4"
lazy val http4sVersion      = "1.0.0-M30"
lazy val jenaVersion        = "4.3.2"
lazy val munitVersion       = "0.7.29"
lazy val munitEffectVersion = "1.0.7"

lazy val rdf4jVersion           = "3.4.2"
lazy val scalaCollCompatVersion = "2.6.0"

// Dependency modules

lazy val utils = "es.weso" %% "utils" % utilsVersion

lazy val catsCore          = "org.typelevel"          %% "cats-core"               % catsVersion
lazy val catsKernel        = "org.typelevel"          %% "cats-kernel"             % catsVersion
lazy val catsEffect        = "org.typelevel"          %% "cats-effect"             % catsEffectVersion
lazy val circeCore         = "io.circe"               %% "circe-core"              % circeVersion
lazy val circeGeneric      = "io.circe"               %% "circe-generic"           % circeVersion
lazy val circeParser       = "io.circe"               %% "circe-parser"            % circeVersion
lazy val decline           = "com.monovore"           %% "decline"                 % declineVersion
lazy val declineEffect     = "com.monovore"           %% "decline-effect"          % declineVersion
lazy val fs2Core           = "co.fs2"                 %% "fs2-core"                % fs2Version
lazy val http4sEmberClient = "org.http4s"             %% "http4s-ember-client"     % http4sVersion
lazy val jenaArq           = "org.apache.jena"         % "jena-arq"                % jenaVersion
lazy val jenaFuseki        = "org.apache.jena"         % "jena-fuseki-main"        % jenaVersion
lazy val munit             = "org.scalameta"          %% "munit"                   % munitVersion
lazy val munitEffects      = "org.typelevel"          %% "munit-cats-effect-3"     % munitEffectVersion
lazy val rdf4j_runtime     = "org.eclipse.rdf4j"       % "rdf4j-runtime"           % rdf4jVersion pomOnly ()
lazy val scalaCollCompat   = "org.scala-lang.modules" %% "scala-collection-compat" % scalaCollCompatVersion

addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full)

ThisBuild / githubWorkflowJavaVersions := Seq(Java11)

def priorTo2_13(scalaVersion: String): Boolean =
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, minor)) if minor < 13 => true
    case _                              => false
  }

lazy val srdfMain = project
  .in(file("."))
  .settings(
    commonSettings
  )
  .aggregate(srdf, srdfJena, srdf4j, docs)
  .dependsOn(srdf, srdfJena)
  .settings(
    libraryDependencies ++= Seq(
      decline,
      declineEffect
    ),
    publish / skip := true,
    ThisBuild / turbo := true
  )

lazy val srdf = project
  .in(file("modules/srdf"))
  .settings(commonSettings)
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
      utils
//      scalaLogging,
//      scalaCollCompat,
    )
  )

lazy val srdfJena = project
  .in(file("modules/srdfJena"))
  .dependsOn(srdf)
  .settings(commonSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
//      logbackClassic % Test,
//      scalaLogging,
      jenaFuseki % Test,
//      typesafeConfig % Test,
      utils,
      jenaArq,
      catsCore,
      catsKernel,
      catsEffect,
      fs2Core,
      http4sEmberClient
    )
  )

lazy val srdf4j = project
  .in(file("modules/srdf4j"))
  .dependsOn(srdf)
  .settings(commonSettings)
  .settings(
    crossScalaVersions := supportedScalaVersions,
    libraryDependencies ++= Seq(
      utils,
      rdf4j_runtime,
      catsCore,
      catsKernel,
      catsEffect,
      fs2Core,
      scalaCollCompat
    )
  )

lazy val docs = project
  .in(file("srdf-docs"))
  .settings(
    noPublishSettings,
    mdocSettings,
    ScalaUnidoc / unidoc / unidocProjectFilter := inAnyProject -- inProjects(noDocProjects: _*)
  )
  .dependsOn(
    srdf,
    srdfJena
//    srdf4j
  )
  .enablePlugins(MdocPlugin, DocusaurusPlugin, ScalaUnidocPlugin)

lazy val mdocSettings = Seq(
  mdocVariables := Map(
    "VERSION" -> version.value
  ),
  ScalaUnidoc / unidoc / unidocProjectFilter := inProjects(srdf, srdfJena, srdf4j),
  ScalaUnidoc / unidoc / target := (LocalRootProject / baseDirectory).value / "website" / "static" / "api",
  cleanFiles += (ScalaUnidoc / unidoc / target).value,
  docusaurusCreateSite := docusaurusCreateSite
    .dependsOn(Compile / unidoc)
    .value,
  docusaurusPublishGhpages :=
    docusaurusPublishGhpages
      .dependsOn(Compile / unidoc)
      .value,
  ScalaUnidoc / unidoc / scalacOptions ++= Seq(
    "-doc-source-url",
    s"https://github.com/weso/srdf/tree/v${(ThisBuild / version).value}€{FILE_PATH}.scala",
    "-sourcepath",
    (LocalRootProject / baseDirectory).value.getAbsolutePath,
    "-doc-title",
    "SRDF",
    "-doc-version",
    s"v${(ThisBuild / version).value}"
  )
)

lazy val noPublishSettings = publish / skip := true

/* ********************************************************
 ******************** Grouped Settings ********************
 **********************************************************/

lazy val noDocProjects = Seq[ProjectReference](
)

lazy val sharedDependencies = Seq(
  libraryDependencies ++= Seq(
    munit        % Test,
    munitEffects % Test
  ),
  testFrameworks += new TestFramework("munit.Framework")
)

lazy val compilationSettings = Seq(
  // format: off
  scalacOptions ++= Seq(
    "-deprecation",                      // Emit warning and location for usages of deprecated APIs.
    "-encoding", "utf-8",                // Specify character encoding used by source files.
    "-feature",                          // Emit warning and location for usages of features that should be imported explicitly.  "-encoding", "UTF-8",
    "-language:_",
    "-Xlint"
  )
  // format: on
)

/*lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  organization := "es.weso",
 )

lazy val publishSettings = Seq(
  homepage            := Some(url("https://github.com/weso/srdf")),
  publishMavenStyle   := true,
  licenses            := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  homepage            := Some(url("https://github.com/weso/srdf")),
  scmInfo             := Some(ScmInfo(url("https://github.com/weso/srdf"), "scm:git:git@github.com:weso/srdf.git")),
  autoAPIMappings     := true,
  pomExtra            := <developers>
                       <developer>
                         <id>labra</id>
                         <name>Jose Emilio Labra Gayo</name>
                         <url>https://github.com/labra/</url>
                       </developer>
                     </developers>,
 ThisBuild / publishTo := {
   val nexus = "https://oss.sonatype.org/"
   if (isSnapshot.value)
     Some("snapshots" at nexus + "content/repositories/snapshots")
   else
     Some("releases"  at nexus + "service/local/staging/deploy/maven2")
 }
)*/

lazy val commonSettings = compilationSettings ++ sharedDependencies ++ Seq(
  coverageHighlighting := priorTo2_13(scalaVersion.value),
  organization := "es.weso",
  sonatypeProfileName := ("es.weso"),
  homepage := Some(url("https://github.com/weso/srdf")),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scmInfo := Some(ScmInfo(url("https://github.com/weso/srdf"), "scm:git:git@github.com:weso/srdf.git")),
  autoAPIMappings := true,
  apiURL := Some(url("http://weso.github.io/utils/latest/api/")),
  autoAPIMappings := true,
  developers := List(
    Developer(
      id = "labra",
      name = "Jose Emilio Labra Gayo",
      email = "jelabra@gmail.com",
      url = url("https://weso.labra.es")
    )
  ),
  resolvers += Resolver.sonatypeRepo("public")
)
