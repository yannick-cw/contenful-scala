import Dependencies._
import CompilerOptions._

name in ThisBuild := "contentful-scala"
organization in ThisBuild := "io.github.yannick-cw"
scalaVersion in ThisBuild := "2.12.8"
scalacOptions in ThisBuild ++= compilerOptions
scalafmtOnCompile in ThisBuild := true

releasePublishArtifactsAction in ThisBuild := PgpKeys.publishSigned.value
publishTo in ThisBuild := Some(
  if (isSnapshot.value)
    Opts.resolver.sonatypeSnapshots
  else
    Opts.resolver.sonatypeStaging
)

lazy val root = (project in file("."))
  .settings(
    publishArtifact := false,
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
  )
  .aggregate(cmaParser)

lazy val cmaParser = (project in file("cma-parser")).settings(
  name := "contentful-cma-parser",
  libraryDependencies := parserDependencies
)
