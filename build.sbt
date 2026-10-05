val scala3Version = "3.9.0"

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

val goldenScalatest = "com.github.j-mie6" %% "golden-scalatest" % "0.1.0-M2"
val parsley = "com.github.j-mie6" %% "parsley" % "5.0.0-M19"
val scalatest = "org.scalatest" %% "scalatest" % "3.2.20" % "test"
val cats = "org.typelevel" %% "cats-core" % "2.13.0"
val catsCollections = "org.typelevel" %% "cats-collections-core" % "0.9.10"

lazy val root = project
  .in(file("."))
  .aggregate(cps, macros, matchtypes)

lazy val cps = project
  .in(file("cps"))
  .settings(
    name := "cps",
    libraryDependencies ++= Seq(
      cats,
      goldenScalatest,
      parsley,
      scalatest,
    )
  )

lazy val macros = project
  .in(file("macros"))
  .settings(
    name := "macros",

    libraryDependencies ++= Seq(
      cats,
      catsCollections,
      goldenScalatest,
      parsley,
      scalatest,
    ),
  )

lazy val matchtypes = project
  .in(file("matchtypes"))
  .settings(
    name := "matchtypes",
    libraryDependencies ++= Seq(
      scalatest,
      cats,
    ),
  )

lazy val benchmark = project
  .in(file("benchmark"))
  .dependsOn(macros)
  .enablePlugins(JmhPlugin)
  .settings(
    name := "benchmark",

    libraryDependencies ++= Seq(
      "org.openjdk.jmh" % "jmh-core" % "1.37",
      "org.openjdk.jmh" % "jmh-generator-annprocess" % "1.37",
    )
  )
