name := "ssimm"

organization := "name.nikiforo"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.11"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.google.jimfs" % "jimfs" % "1.1" % "test"
)