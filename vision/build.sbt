name := "SoftRainsVision"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

libraryDependencies ++= Seq(
  "com.meetup" %% "archery" % "0.4.0"
)

javaCppPresetLibs ++= Seq(
  "ffmpeg" -> "3.1.2"
)

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Common.akkaDeps

parallelExecution in Test := false

enablePlugins(JavaAppPackaging)

mappings in (Compile, packageDoc) := Seq()
mappings in (Compile, packageSrc) := Seq()
