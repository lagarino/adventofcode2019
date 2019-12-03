name := "adventOfCode2019"

version := "0.1"

scalaVersion := "2.13.1"

val scalaTest   = "org.scalatest" %% "scalatest" % "3.1.0" % Test

libraryDependencies += scalaTest

credentials ++= Seq(
  Credentials(Path.userHome / ".sbt" / ".bintrayCredentials"),
  Credentials(Path.userHome / ".sbt" / ".nexusCredentials")
)

resolvers ++= Seq(
  "Sonatype Nexus Repository Manager" at "https://maven.zalando.net/content/repositories/releases",
  Resolver.bintrayRepo("dehora", "maven")
)