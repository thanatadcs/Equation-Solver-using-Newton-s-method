val scala3Version = "3.1.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Basic Equation Solver",
    version := "0.0.1",

    scalaVersion := scala3Version,
    libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.0"
  )
