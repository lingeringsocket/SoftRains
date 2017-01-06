classpathTypes += "maven-plugin"

libraryDependencies += "org.bytedeco" % "javacpp" % "1.2"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-start-script" % "0.10.0")
