name := "SoftRainsCentral"

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
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "javax.mail" % "mail" % "1.4.7",
  "com.meetup" %% "archery" % "0.4.0",
  "nu.validator" % "htmlparser" % "1.4.4",
  "org.sorm-framework" % "sorm" % "0.3.20",
  "com.h2database" % "h2" % "1.3.168",
  "com.typesafe.akka" %% "akka-http" % "10.0.0",
  "org.postgresql" % "postgresql" % "9.4.1210"
)

libraryDependencies ++= Common.javacvDeps

libraryDependencies ++= Common.javacvPlatformDeps()

libraryDependencies ++= Common.ffmpegPlatformDeps()

libraryDependencies ++= Common.specs2Deps

libraryDependencies ++= Common.akkaDeps

parallelExecution in Test := false
