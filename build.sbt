enablePlugins(JavaAppPackaging)

enablePlugins(com.mpc.scalats.sbt.TypeScriptGeneratorPlugin)

name := "akka-http-microservice"
organization := "com.theiterators"
version := "1.0"
scalaVersion := "2.11.8"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8")

mainClass := Some("org.scalajs.tools.tsimporter.Main")

libraryDependencies ++= {
  val akkaV       = "2.4.12"
  val akkaHttpV   = "3.0.0-RC1"
  val scalaTestV  = "3.0.1"
  Seq(
    "com.typesafe.akka" %% "akka-actor" % akkaV,
    "com.typesafe.akka" %% "akka-stream" % akkaV,
    "com.typesafe.akka" %% "akka-testkit" % akkaV,
    "com.typesafe.akka" %% "akka-http" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpV,
    "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpV,
    "org.scalatest"     %% "scalatest" % scalaTestV , //% "test",
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "ch.megard" %% "akka-http-cors" % "0.1.10",

//  "com.google.code.gson" % "gson" % "2.3.1",
//  "com.squareup.okhttp" % "okhttp" % "2.3.0",
//  "com.squareup.okio" % "okio" % "1.0.0",
  "com.qiniu"%"qiniu-java-sdk"%"7.2.2"
  )
}

Revolver.settings
//libraryDependencies +=

//organization := "org.scalajs.tools"

//scalacOptions ++= Seq(
//  "-deprecation",
//  "-unchecked",
//  "-feature",
//  "-encoding", "utf8"
//)

resolvers := Seq(
  Resolver.mavenLocal,
  "Sonatype " at "http://maven.aliyun.com/nexus/content/groups/public/",
  "typesafe" at  "http://repo.typesafe.com/typesafe/ivy-releases/",
  "jla" at "http://repo.akka.io/snapshots/"

)

fullResolvers := Seq(
  Resolver.mavenLocal,
  "Sonatype " at "http://maven.aliyun.com/nexus/content/groups/public/",
  "typesafe" at  "http://repo.typesafe.com/typesafe/ivy-releases/",
  "jla" at "http://repo.akka.io/snapshots/"
)

addCompilerPlugin("org.psywerx.hairyfotr" %% "linter" % "0.1.17")
