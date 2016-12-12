scalaVersion := "2.11.7"

name := "DumpFramer"

version := "0.1-SNAPSHOT"

mainClass := Some("org.scalajs.tools.tsimporter.Main")

libraryDependencies +=
  "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.2"

organization := "org.scalajs.tools"

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-encoding", "utf8"
)

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
