import org.scalajs.linker.interface.ModuleKind
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.*
import org.typelevel.sbt.gha.JavaSpec
import sbtcrossproject.{CrossProject, CrossType}
import sbtcrossproject.CrossPlugin.autoImport.*
import scalajscrossproject.ScalaJSCrossPlugin.autoImport.*

val Scala3 = "3.7.4"
val munitV = "1.3.4"
val munitCheckV = "1.3.0"
val scalaCheckV = "1.19.0"

lazy val locus4sRevision =
  "eec9a4b9527f7f6a62256dcf45a47e14bd2c7fb5"
lazy val graph4sRevision =
  "ea5d2d762f85f5a0f97ee188deb5fac0ef2bcbaf"
lazy val spatial4sRevision =
  "1b98abb873543f90a1e532e1a67f39e16c1ba09d"

def sourceBuild(
    property: String,
    repository: String,
    revision: String
) =
  sys.props
    .get(property)
    .map(path => file(path).getCanonicalFile.toURI)
    .getOrElse(uri(s"$repository#$revision"))

def sourceDescription(
    property: String,
    repository: String,
    revision: String
): String =
  sys.props
    .get(property)
    .map(path => s"local:${file(path).getCanonicalPath}")
    .getOrElse(s"$repository#$revision")

lazy val locus4sBuild =
  sourceBuild(
    "mesh4s.locus4s.build",
    "https://github.com/canardlapin/locus4s.git",
    locus4sRevision
  )
lazy val locus4sCoreJVM = ProjectRef(locus4sBuild, "locus4s-coreJVM")
lazy val locus4sCoreJS = ProjectRef(locus4sBuild, "locus4s-coreJS")
lazy val locus4sDataJVM = ProjectRef(locus4sBuild, "locus4s-dataJVM")
lazy val locus4sDataJS = ProjectRef(locus4sBuild, "locus4s-dataJS")

lazy val spatial4sBuild =
  sourceBuild(
    "mesh4s.spatial4s.build",
    "https://github.com/canardlapin/spatial4s.git",
    spatial4sRevision
  )
lazy val spatial4sCoreJVM = ProjectRef(spatial4sBuild, "spatial4s-coreJVM")
lazy val spatial4sCoreJS = ProjectRef(spatial4sBuild, "spatial4s-coreJS")

lazy val graph4sBuild =
  sourceBuild(
    "mesh4s.graph4s.build",
    "https://github.com/canardlapin/graph4s.git",
    graph4sRevision
  )
lazy val graph4sCoreJVM = ProjectRef(graph4sBuild, "coreJVM")
lazy val graph4sCoreJS = ProjectRef(graph4sBuild, "coreJS")
lazy val graph4sIndexedJVM = ProjectRef(graph4sBuild, "indexedJVM")
lazy val graph4sIndexedJS = ProjectRef(graph4sBuild, "indexedJS")

ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "io.github.canardlapin"
ThisBuild / organizationName := "Bradley Buchsbaum"
ThisBuild / startYear := Some(2026)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(
  tlGitHubDev("canardlapin", "Bradley Buchsbaum")
)
ThisBuild / homepage := Some(url("https://github.com/canardlapin/mesh4s"))

ThisBuild / scalaVersion := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)
ThisBuild / tlJdkRelease := Some(11)
ThisBuild / githubWorkflowJavaVersions := Seq(
  JavaSpec.temurin("17"),
  JavaSpec.temurin("21")
)
ThisBuild / versionPolicyIntention := Compatibility.None
ThisBuild / tlFatalWarnings := true

lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-Werror",
    "-Wconf:msg=package scala contains object and package with same name.*caps:silent"
  ),
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % munitV % Test,
    "org.scalameta" %%% "munit-scalacheck" % munitCheckV % Test
  ),
  Test / parallelExecution := false
)

def meshProject(artifact: String): CrossProject =
  CrossProject(artifact, file(s"modules/$artifact"))(JSPlatform, JVMPlatform)
    .crossType(CrossType.Full)
    .settings(commonSettings)
    .settings(name := artifact)
    .jsSettings(
      scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule))
    )

lazy val mesh4sCore =
  meshProject("mesh4s-core")
    .jvmConfigure(_.dependsOn(locus4sCoreJVM, locus4sDataJVM))
    .jsConfigure(_.dependsOn(locus4sCoreJS, locus4sDataJS))

lazy val mesh4sGeometry =
  meshProject("mesh4s-geometry")
    .dependsOn(mesh4sCore)
    .jvmConfigure(_.dependsOn(spatial4sCoreJVM))
    .jsConfigure(_.dependsOn(spatial4sCoreJS))

lazy val mesh4sReference =
  meshProject("mesh4s-reference")
    .dependsOn(mesh4sCore, mesh4sGeometry)

lazy val mesh4sLaws =
  meshProject("mesh4s-laws")
    .dependsOn(
      mesh4sCore,
      mesh4sGeometry,
      mesh4sReference,
      mesh4sGraph4s
    )
    .settings(
      libraryDependencies +=
        "org.scalacheck" %%% "scalacheck" % scalaCheckV
    )

lazy val mesh4sGraph4s =
  meshProject("mesh4s-graph4s")
    .dependsOn(mesh4sCore, mesh4sGeometry)
    .jvmConfigure(
      _.dependsOn(graph4sCoreJVM, graph4sIndexedJVM)
    )
    .jsConfigure(
      _.dependsOn(graph4sCoreJS, graph4sIndexedJS)
    )

lazy val docs =
  project
    .in(file("site"))
    .dependsOn(mesh4sCore.jvm, mesh4sGeometry.jvm, mesh4sGraph4s.jvm)
    .enablePlugins(TypelevelSitePlugin)
    .settings(
      name := "mesh4s-docs",
      description := "Executable guides and reference documentation for mesh4s.",
      publish / skip := true,
      mdocExtraArguments += "--no-link-hygiene"
    )

lazy val dependencyPins = taskKey[Unit](
  "Print the immutable dependency revisions or explicit local overrides."
)

Global / dependencyPins := {
  val log = streams.value.log
  log.info(
    "locus4s=" +
      sourceDescription(
        "mesh4s.locus4s.build",
        "https://github.com/canardlapin/locus4s.git",
        locus4sRevision
      )
  )
  log.info(
    "graph4s=" +
      sourceDescription(
        "mesh4s.graph4s.build",
        "https://github.com/canardlapin/graph4s.git",
        graph4sRevision
      )
  )
  log.info(
    "spatial4s=" +
      sourceDescription(
        "mesh4s.spatial4s.build",
        "https://github.com/canardlapin/spatial4s.git",
        spatial4sRevision
      )
  )
}

lazy val root =
  project
    .in(file("."))
    .aggregate(
      mesh4sCore.jvm,
      mesh4sCore.js,
      mesh4sGeometry.jvm,
      mesh4sGeometry.js,
      mesh4sReference.jvm,
      mesh4sReference.js,
      mesh4sLaws.jvm,
      mesh4sLaws.js,
      mesh4sGraph4s.jvm,
      mesh4sGraph4s.js,
      docs
    )
    .settings(
      name := "mesh4s-root",
      publish / skip := true
    )

addCommandAlias("compileAll", ";root/compile")
addCommandAlias("testAll", ";root/test")
addCommandAlias(
  "testFullOptJS",
  ";set Global / scalaJSStage := FullOptStage;" +
    "mesh4s-coreJS/test;" +
    "mesh4s-geometryJS/test;" +
    "mesh4s-referenceJS/test;" +
    "mesh4s-lawsJS/test;" +
    "mesh4s-graph4sJS/test"
)
addCommandAlias(
  "checkAll",
  ";scalafmtCheckAll;scalafmtSbtCheck;dependencyPins;compileAll;testAll"
)
addCommandAlias(
  "docsCheck",
  ";mesh4s-coreJVM/doc;" +
    "mesh4s-geometryJVM/doc;" +
    "mesh4s-referenceJVM/doc;" +
    "mesh4s-lawsJVM/doc;" +
    "mesh4s-graph4sJVM/doc;" +
    "docs/tlSite"
)
