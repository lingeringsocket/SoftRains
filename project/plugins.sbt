classpathTypes += "maven-plugin"

resolvers += Resolver.url(
  "bintray-gilt-sbt-plugin-releases",
  url("http://dl.bintray.com/giltgroupe/sbt-plugin-releases"))(
      Resolver.ivyStylePatterns)

libraryDependencies += "org.bytedeco" % "javacpp" % "1.2"

addSbtPlugin("org.scalastyle" %% "scalastyle-sbt-plugin" % "0.8.0")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.8.2")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.1.4")

addSbtPlugin("com.gilt.sbt" % "sbt-alpn" % "0.0.5")
