name := "SoftRainsRoot"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalastyleFailOnError := true

lazy val base = project

lazy val intercom = project.dependsOn(base % "test->test;compile->compile")

lazy val vision = project.dependsOn(base % "test->test;compile->compile")

lazy val central = project.dependsOn(
  intercom % "test->test;compile->compile",
  vision % "test->test;compile->compile")
