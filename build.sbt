
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "com.ibm.db2" % "jcc" % "11.5.8.0"
libraryDependencies += "org.postgresql" % "postgresql" % "42.7.7"
Compile / mainClass := Some("org.pwharned.main")

//enablePlugins(ScalaNativePlugin)
//nativeMode:= "release-fast"
enablePlugins(GraalVMNativeImagePlugin)
lazy val caseClassGenerator = project.in(file("caseClassGenerator"))
  .settings(
    name := "caseClassGenerator",
    scalaVersion := "2.13.16"
  )
graalVMNativeImageOptions ++= Seq(
  "--allow-incomplete-classpath",
  "-H:ResourceConfigurationFiles=../../resource-config.json",

)

ThisBuild/ scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-unchecked"
)
scalacOptions ++= Seq("-Xmax-inlines", "50")

lazy val root = project.in(file("."))
  .settings(
    name := "crassus",
    scalaVersion := "3.7.0",
    Compile / sourceGenerators += Def.task {
      val outputDir = baseDirectory.value / "src/main/scala/org/pwharned/generated"

      // Ensure caseClassGenerator is compiled first
      val _ = (caseClassGenerator / Compile / compile).value

      // Load the compiled class dynamically
      val classpath = (caseClassGenerator / Compile / dependencyClasspath).value
      val classDir = (caseClassGenerator / Compile / classDirectory).value

      // Create a classloader that includes both your compiled classes and all dependencies

      val urls = classpath.map(_.data.toURI.toURL).toArray :+ classDir.toURI.toURL

      val classLoader = new java.net.URLClassLoader(urls)
      println(s"Compiled classes are in: $classDir")
      println("Available classes: " + classLoader.getResources(""))
      val generatorClass = classLoader.loadClass("org.pwharned.generator.CaseClassGenerator")
      val method = generatorClass.getMethod("generateCaseClasses")
      val generatedCode = method.invoke(null).toString
      val code  =
        s"""
           |package generated
           |import org.pwharned.database.HKD._
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
