
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
    scalaVersion := "2.13.16",
    publish / skip := false
  )





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

    Compile / scalacOptions ++= Seq(
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
"-Xmax-inlines", "64",
     // "-Xprint-suspension",
    ),

      Compile / sourceGenerators += Def.task {
      val outputDir = baseDirectory.value / "src/main/scala/org/pwharned/generated"
      val base      = baseDirectory.value
      // Ensure caseClassGenerator is compiled first
      val _ = (caseClassGenerator / Compile / compile).value
      val schema    = base / "schema.sql"                // locate your schema file
      // Load the compiled class dynamically
      val classpath = (caseClassGenerator / Compile / dependencyClasspath).value
      val classDir = (caseClassGenerator / Compile / classDirectory).value

      // Create a classloader that includes both your compiled classes and all dependencies

      val urls = classpath.map(_.data.toURI.toURL).toArray :+ classDir.toURI.toURL

      val classLoader = new java.net.URLClassLoader(urls)
      println(s"Compiled classes are in: $classDir")
      println("Available classes: " + classLoader.getResources(""))
      println("Path to schema file is " + schema.getAbsolutePath)
      val generatorClass = classLoader.loadClass("org.pwharned.generator.CaseClassGenerator")
      val method   = generatorClass.getMethod("generateCaseClasses", classOf[String])
      val generatedCode    = method.invoke(null, schema.getAbsolutePath).asInstanceOf[String]
      val code  =
        s"""
           |package generated
           |import org.pwharned.sql.database.HKD._
           |import java.sql.Timestamp
           |
           |$generatedCode
           |"""
          .stripMargin

      val file = outputDir / "Generated.scala"

      IO.write(file, code)

      println(s"Generated source file: $file")
      Seq(file)
    }.taskValue
  )
  .dependsOn(caseClassGenerator)
