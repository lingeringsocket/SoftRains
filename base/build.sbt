name := "SoftRainsBase"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

libraryDependencies ++= Seq(
  "com.jsuereth" %% "scala-arm" % "1.4"
)
libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Common.akkaDeps
