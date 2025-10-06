import org.pwharned.generator.CaseClassGenerator

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

lazy val caseClassGenerator = project.in(file("caseClassGenerator"))
  .settings(
    name := "caseClassGenerator",
    ThisBuild / organization := "org.pwharned",
      scalaVersion := "2.12.18",
    publish / skip := false,
    sbtPlugin := true,        // <--- required
  )  .enablePlugins(SbtPlugin)


lazy val excludedPrefixes = Seq(
  "generated","main"
)
lazy val generateModels = taskKey[Unit]("Generate case classes from SQL schema")

generateModels := {
  val output = CaseClassGenerator.generateCaseClasses("src/main/resources/schema.sql")
  val finalOutput = s"""
                       |package com.myapp.models
                       |
                       |import java.sql.Timestamp
                       |
                       |$output
                       |""".stripMargin

  IO.write(file("src/main/scala/org/pwharned/models/Generated.scala"), finalOutput)
  println("✅ Generated case classes successfully!")
}


lazy val root = project.in(file(".")).enablePlugins(CaseClassGeneratorPlugin)

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
