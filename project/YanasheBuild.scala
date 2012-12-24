import sbt._
import sbt.Keys._

object YanasheBuild extends Build {

  lazy val yanashe = Project(
    id = "yanashe",
    base = file("."),
    settings = Project.defaultSettings ++ Seq(
      name := "yanashe",
      organization := "org.logginging",
      version := "0.1-SNAPSHOT",
      scalaVersion := "2.9.2",
      // add other settings here
      resolvers ++= Seq(
        "Spray Repository" at "http://repo.spray.io",
        "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases"
      ),
      libraryDependencies ++= Seq(
        "org.twitter4j" % "twitter4j-core" % "3.0.2",
        "org.twitter4j" % "twitter4j-stream" % "3.0.2",
        "io.spray" %%  "spray-json" % "1.2.3" cross CrossVersion.full,
        "org.scalatest" %% "scalatest" % "1.6.1" % "test"
      )
    )
  )
}
