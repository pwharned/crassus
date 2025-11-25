
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "com.ibm.db2" % "jcc" % "11.5.8.0"
libraryDependencies += "org.postgresql" % "postgresql" % "42.7.7"
Compile / mainClass := Some("org.pwharned.main")
Global / parallelExecution := true



lazy val caseClassGenerator = project.in(file("caseClassGenerator"))
  .settings(

    name := "caseClassGenerator",
    ThisBuild / organization := "org.pwharned",
      scalaVersion := "2.12.18",
    publish / skip := false,
    publishTo := {
      sys.props.get("publish.repo") match {
        case Some(path) => Some("Custom Local Repo" at s"file://$path")
        case None       => None // fallback to default or error
      }
    },
      sbtPlugin := true,        // <--- required
  )  .enablePlugins(SbtPlugin)


lazy val excludedPrefixes = Seq(
  "generated","main"
)
lazy val generateModels = taskKey[Unit]("Generate case classes from SQL schema")



lazy val root = project.in(file("."))

  .settings(
    name := "crassus",
    scalaVersion := "3.7.1",
    libraryDependencies += "org.scala-lang" % "scala3-library_3" % scalaVersion.value,
    scalacOptions ++= Seq(
      "-Xlog-implicits",   // see implicit resolution attempts
      "-Xprint:typer",     // show expanded code after typer phase
      "-Ystatistics"       // show phase timings to spot hotspots
    ),

      ThisBuild / logLevel := Level.Info,
      // in build.sbt
    Compile / compile / fork := true,
      Compile / compile / javaOptions ++= Seq(
   //   "-Xms4G", "-Xmx8G", "-XX:+UseG1GC"
    ),
      Compile / scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
"-Xmax-inlines", "64",
     // "-Xprint-suspension",
    )
  )
