name := "SoftRainsAlexa"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

libraryDependencies ++= Seq(
  "com.amazon.alexa.avs" % "sample-java-client" % "20160207.1"
)

enablePlugins(JavaAppPackaging)

enablePlugins(JettyAlpn)
