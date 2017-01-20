name := "SoftRainsKiosk"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

enablePlugins(JavaAppPackaging)

mappings in (Compile, packageDoc) := Seq()
