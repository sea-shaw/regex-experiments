val scala3Version = "3.8.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "experiments",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "1.3.2" % Test,
      "org.typelevel" %% "cats-collections-core" % "0.9.10",
    )
  )
