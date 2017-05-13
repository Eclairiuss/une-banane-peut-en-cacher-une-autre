name := "THUD"

version := "1.0"

scalaVersion := "2.12.1"

unmanagedResourceDirectories in Compile += baseDirectory.value / "resources"
unmanagedResourceDirectories in Test += baseDirectory.value / "resources"
unmanagedResourceDirectories in Runtime += baseDirectory.value / "resources"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"
