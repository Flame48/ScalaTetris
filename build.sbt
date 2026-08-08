val scala3Version = "3.8.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "tetris",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.jline" % "jline" % "3.26.3",
      "org.scalameta" %% "munit" % "1.3.4" % Test
    )
  )
