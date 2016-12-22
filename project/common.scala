// SoftRains:  a Genuine People Personality for your home
// Copyright 2016 John V. Sichi
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

import sbt._
import Keys._

object Common {
  def organization = "com.lingeringsocket.softrains"

  def version = "0.1"

  def scalaVersion = "2.11.8"

  def resolvers = Seq(
    Resolver.mavenLocal,
    DefaultMavenRepository,
    Resolver.typesafeRepo("releases"),
    Resolver.typesafeRepo("snapshots"),
    Resolver.typesafeIvyRepo("snapshots"),
    Resolver.sonatypeRepo("releases"),
    Resolver.sonatypeRepo("snapshots"),
    Resolver.defaultLocal,
    bintray.Opts.resolver.jcenter)

  def opencvVersion = "3.1.0"

  def javacppVersion = "1.2"

  def javacppPointVersion = "1.2.5"

  def ffmpegVersion = "3.0.2"

  lazy val defaultPlatform = org.bytedeco.javacpp.Loader.getPlatform

  def scalacOptions = Seq(
    "-unchecked", "-feature", "-Xlint", "-Ywarn-unused-import",
    "-deprecation", "-Xfatal-warnings", "-Yrangepos")

  def specs2Deps = Seq(
    "org.specs2" %% "specs2-core" % "3.8.5" % "test")

  def akkaDeps = Seq(
    "com.typesafe.akka" %% "akka-actor" % "2.4.14",
    "com.typesafe.akka" %% "akka-remote" % "2.4.14" exclude("io.netty", "netty"),
    "com.typesafe.akka" %% "akka-testkit" % "2.4.14" % "test")

  def javacvDeps = Seq(
    "org.bytedeco" % "javacv" % javacppVersion,
    "org.bytedeco" % "javacpp" % javacppPointVersion)

  def javacvPlatformDeps(
    scope : String = "compile", platform : String = defaultPlatform) =
    Seq(
      "org.bytedeco.javacpp-presets" % "opencv" %
        (opencvVersion + "-" + javacppVersion) % scope classifier "",
      "org.bytedeco.javacpp-presets" % "opencv" %
        (opencvVersion + "-" + javacppVersion) % scope classifier platform)

  def ffmpegPlatformDeps(
    scope : String = "compile", platform : String = defaultPlatform) =
    Seq(
      "org.bytedeco.javacpp-presets" % "ffmpeg" %
        (ffmpegVersion + "-" + javacppVersion) % scope classifier "",
      "org.bytedeco.javacpp-presets" % "ffmpeg" %
        (ffmpegVersion + "-" + javacppVersion) % scope classifier platform)

  def maxErrors = 99

  def traceLevel = 10
}