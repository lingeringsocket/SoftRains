name := "SoftRainsCentral"

scalacOptions := Seq(
  "-unchecked", "-feature", "-Xlint", "-Ywarn-unused-import",
  "-deprecation", "-Xfatal-warnings")

seq(webSettings :_*)

val liftVersion = "3.0-RC3"

val opencvVersion = "3.1.0"

val javacppVersion = "1.2"

val javacppPointVersion = "1.2.1"

val ffmpegVersion = "3.0.2"

val platform = org.bytedeco.javacpp.Loader.getPlatform

autoCompilerPlugins := true

classpathTypes += "maven-plugin"

libraryDependencies ++= Seq(
  "net.liftweb" %% "lift-webkit" % liftVersion % "compile",
  "net.liftmodules" %% "lift-jquery-module_3.0" % "2.10",
  "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910"  %
    "container,test",
  "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" %
    "container,compile" artifacts Artifact("javax.servlet", "jar", "jar"),
  "javax.mail" % "mail" % "1.4",
  "net.databinder.dispatch" %% "dispatch-core" % "0.11.2",
  "com.meetup" %% "archery" % "0.4.0",
  "nu.validator" % "htmlparser" % "1.4.4",
  "org.bytedeco" % "javacv" % javacppVersion,
  "org.bytedeco" % "javacpp" % javacppPointVersion,
  "org.bytedeco.javacpp-presets" % "opencv" %
    (opencvVersion + "-" + javacppVersion) % "compile" classifier "",
  "org.bytedeco.javacpp-presets" % "opencv" %
    (opencvVersion + "-" + javacppVersion) % "compile" classifier platform,
  "org.bytedeco.javacpp-presets" % "ffmpeg" %
    (ffmpegVersion + "-" + javacppVersion) % "compile" classifier "",
  "org.bytedeco.javacpp-presets" % "ffmpeg" %
    (ffmpegVersion + "-" + javacppVersion) % "compile" classifier platform,
  "com.jsuereth" %% "scala-arm" % "1.4",
  "org.sorm-framework" % "sorm" % "0.3.20",
  "com.h2database" % "h2" % "1.3.168",
  "org.postgresql" % "postgresql" % "9.4.1210",
  "com.typesafe.akka" %% "akka-actor" % "2.4.10",
  "com.typesafe.akka" %% "akka-testkit" % "2.4.10" % "test",
  "org.specs2" %% "specs2-core" % "3.7.2" % "test"
)

scalaVersion := "2.11.8"

parallelExecution in Test := false

scalastyleFailOnError := true
