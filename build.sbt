lazy val amyc = (project in file("."))
  .disablePlugins(plugins.JUnitXmlReportPlugin)
  .enablePlugins(StudentPlugin)
  .settings(
    name := "amyc",

    version := "1.7",
    organization := "ch.epfl.lara",
    scalaVersion := "2.12.10",

    scalaSource in Compile := baseDirectory.value / "src",
    scalacOptions ++= Seq("-feature"),

    scalaSource in Test := baseDirectory.value / "test" / "scala",
    parallelExecution in Test := false,
    libraryDependencies += "com.novocode" % "junit-interface" % "0.11" % "test",
    testOptions += Tests.Argument(TestFrameworks.JUnit, "-v")
  )
