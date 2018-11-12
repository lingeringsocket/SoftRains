name := "SoftRainsIntercom"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

libraryDependencies ++= Seq(
  "commons-io" % "commons-io" % "2.5",
  "com.google.code.findbugs" % "jsr305" % "3.0.2",
  "com.ibm.watson.developer_cloud" % "core" % "6.9.1",
  "com.ibm.watson.developer_cloud" % "text-to-speech" % "6.9.1",
  "com.ibm.watson.developer_cloud" % "speech-to-text" % "6.9.1"
)

enablePlugins(JavaAppPackaging)

mappings in (Compile, packageDoc) := Seq()
mappings in (Compile, packageSrc) := Seq()
