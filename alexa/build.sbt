name := "SoftRainsAlexa"

organization := Common.organization

version := Common.version

scalaVersion := Common.scalaVersion

scalacOptions := Common.scalacOptions

resolvers ++= Common.resolvers

maxErrors := Common.maxErrors

traceLevel := Common.traceLevel

scalastyleFailOnError := true

val alexaJavaClientVersion = {
  if (System.getProperty("os.arch", "amd64").startsWith("arm")) {
    "20160207.0"
  } else {
    "20160207.1"
  }
}

libraryDependencies ++= Seq(
  "com.amazon.alexa.avs" % "sample-java-client" % alexaJavaClientVersion
)

enablePlugins(JavaAppPackaging)

enablePlugins(JettyAlpn)

mappings in (Compile, packageDoc) := Seq()
mappings in (Compile, packageSrc) := Seq()
