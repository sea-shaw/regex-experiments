val scala3Version = "3.8.4"

lazy val root = project
  .in(file("."))
  .settings(
    name := "experiments",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "com.github.j-mie6" %% "golden-scalatest" % "0.1.0-M2",
      "com.github.j-mie6" %% "parsley" % "5.0.0-M19",
      "com.github.j-mie6" %% "parsley-cats" % "1.5.0",
      "com.lihaoyi" %% "os-lib" % "0.11.7",
      "org.scalactic" %% "scalactic" % "3.2.20",
      "org.scalatest" %% "scalatest" % "3.2.20" % "test",
      "org.typelevel" %% "cats-core" % "2.13.0",
      "org.typelevel" %% "cats-collections-core" % "0.9.10",
    ),

    libraryDependencySchemes += "com.github.j-mie6" %% "parsley" % VersionScheme.Always,

    scalacOptions ++= Seq(
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
    ),

    Compile / sourceGenerators += (Compile / sourceManaged).map(tuples.gen).taskValue
  )
