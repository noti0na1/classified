ThisBuild / scalaVersion := "3.8.5-RC1-bin-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    name := "classified",

    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-unchecked",
      "-Yexplicit-nulls",
      "-Wunused:all",
      "-Wsafe-init",
      "-language:experimental.captureChecking",
      "-language:experimental.modularity",
      "-language:implicitConversions",
      "-release:17",
    ),

    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit"          % "1.3.0"             % Test,
      "org.scala-lang" %% "scala3-compiler" % scalaVersion.value % Test
    ),

    // We invoke `dotty.tools.dotc.Main` directly from inside the tests,
    // so the test JVM needs the project's classpath made visible via a
    // system property. Forking keeps stdout / stderr from the embedded
    // compilations from polluting sbt's own logger streams.
    Test / fork := true,
    Test / javaOptions ++= Seq(
      s"-Dclassified.classpath=${(Test / fullClasspath).value.map(_.data.getAbsolutePath).mkString(java.io.File.pathSeparator)}"
    )
  )
