val scala3Version = "3.8.4"

Global / semanticdbEnabled := true

ThisBuild / scalaVersion := scala3Version

ThisBuild / scalacOptions ++= Seq(
  "-Yexplicit-nulls",
  "-Xcheck-macros",
  "-explain",
  "-deprecation",
  "-unchecked",
  "-Wimplausible-patterns",
  "-Wunused:all",
  "-Wsafe-init",
  "-feature",
  "-explain-cyclic",
  // "-Vprint:postInlining", // Enable and use `console` to better see generated code
)

lazy val root = project
  .in(file("."))
  .aggregate(macros, matchtypes)

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macros",

    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "golden-scalatest" % "0.1.0-M2",
      "com.github.j-mie6" %% "parsley" % "5.0.0-M19",
      "com.github.j-mie6" %% "parsley-cats" % "1.5.0",
      "org.scalatest" %% "scalatest" % "3.2.20" % "test",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.10",
    ),

    libraryDependencySchemes += "com.github.j-mie6" %% "parsley" % VersionScheme.Always,
  )

lazy val matchtypes = project
  .in(file("matchtypes"))
  .settings(
    name := "matchtypes",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.20" % "test",
      "org.typelevel" %% "cats-core" % "2.13.0",
    ),
  )
