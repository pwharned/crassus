
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "com.ibm.db2" % "jcc" % "11.5.8.0"
libraryDependencies += "org.postgresql" % "postgresql" % "42.7.7"
Compile / mainClass := Some("org.pwharned.main")
Global / parallelExecution := true

//enablePlugins(ScalaNativePlugin)
//nativeMode:= "release-fast"
//enablePlugins(GraalVMNativeImagePlugin)
//graalVMNativeImageOptions ++= Seq(
//  "--allow-incomplete-classpath",
//  "-H:ResourceConfigurationFiles=../../resource-config.json",

//)
import pl.project13.scala.sbt.JmhPlugin

lazy val caseClassGenerator = project.in(file("caseClassGenerator"))
  .settings(
    name := "caseClassGenerator",
    ThisBuild / organization := "org.pwharned",
      scalaVersion := "2.12.18",
    publish / skip := false
  )  .enablePlugins(SbtPlugin)




lazy val excludedPrefixes = Seq(
  "generated","main"
)

val circeVersion = "0.14.14"


lazy val root = project.in(file(".")).enablePlugins(JmhPlugin)

  .settings(
    name := "crassus",
    scalaVersion := "3.7.1",
    Compile / packageBin / mappings := {
      val original: Seq[(File, String)] = (Compile / packageBin / mappings).value
      original.filterNot { case (_, pathInJar) =>
        excludedPrefixes.exists(pathInJar.contains)
      }
    },
    libraryDependencies += "org.scala-lang" % "scala3-library_3" % scalaVersion.value,

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion),
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
