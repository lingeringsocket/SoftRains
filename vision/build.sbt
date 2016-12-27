name := "SoftRainsVision"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

classpathTypes += "maven-plugin"

resolvers ++= Common.resolvers

val platform = org.bytedeco.javacpp.Loader.getPlatform

autoCompilerPlugins := true

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

libraryDependencies ++= Seq(
  "com.meetup" %% "archery" % "0.4.0"
)

libraryDependencies ++= Common.javacvDeps

libraryDependencies ++= Common.javacvPlatformDeps()

libraryDependencies ++= Common.ffmpegPlatformDeps()

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Common.akkaDeps

parallelExecution in Test := false
