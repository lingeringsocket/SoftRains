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
  "com.ibm.watson.developer_cloud" % "text-to-speech" % "3.5.1",
  "com.ibm.watson.developer_cloud" % "speech-to-text" % "3.5.1"
)

mappings in (Compile, packageDoc) := Seq()
