
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
    publish / skip := false
  )  .enablePlugins(SbtPlugin)






lazy val excludedPrefixes = Seq(
  "generated","main"
)



lazy val root = project.in(file("."))
  .settings(
    name := "crassus",
    scalaVersion := "3.7.1",
    Compile / packageBin / mappings := {
      val original: Seq[(File, String)] = (Compile / packageBin / mappings).value
      original.filterNot { case (_, pathInJar) =>
        excludedPrefixes.exists(pathInJar.contains)
      }
    },
    // in build.sbt
    Compile / compile / fork := true,
      Compile / compile / javaOptions ++= Seq(
      "-Xms4G", "-Xmx8G", "-XX:+UseG1GC"
    ),

    libraryDependencies += "io.circe" %% "circe-core" % "0.15.0-M1",
    libraryDependencies += "io.circe" %% "circe-generic" % "0.15.0-M1",
    libraryDependencies += "io.circe" %% "circe-parser" % "0.15.0-M1",

      Compile / scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
"-Xmax-inlines", "64",
     // "-Xprint-suspension",
    )
  )
