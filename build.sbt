name := "SoftRainsRoot"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

lazy val base = project

lazy val intercom = project.dependsOn(base % "test->test;compile->compile")

lazy val vision = project.dependsOn(base % "test->test;compile->compile")

lazy val kiosk = project.dependsOn(
  intercom % "test->test;compile->compile",
  vision % "test->test;compile->compile")

lazy val central = project.dependsOn(
  intercom % "test->test;compile->compile")

// optional component, so we don't include it in root
lazy val alexa = project.dependsOn(kiosk % "test->test;compile->compile")

lazy val root = (project in file(".")).aggregate(
  base, intercom, vision, kiosk, central)

mappings in (Compile, packageDoc) := Seq()
mappings in (Compile, packageSrc) := Seq()
