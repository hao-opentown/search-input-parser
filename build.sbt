lazy val commonSettings = Seq(
	organization := "com.example",
	version := "0.1.0",
	scalaVersion := "2.11.4"
)

lazy val root = (project in file(".")).
settings(commonSettings: _*).
settings(
	name := "hello",
	libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
)
