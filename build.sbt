name := "MTG Challenge"

version := "1.0"

scalaVersion := "2.10.3"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

parallelExecution in Test := false

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
