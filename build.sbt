lazy val root = (project in file(".")).
  settings(
    name := "gcl_parser",
    version := "0.0.1",
    scalaVersion := "2.11.4",
    libraryDependencies += "org.scala-lang.modules" %
      "scala-parser-combinators_2.11" % "1.0.3"
  )

