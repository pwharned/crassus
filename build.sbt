
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "com.ibm.db2" % "jcc" % "11.5.8.0"
lazy val caseClassGenerator = project.in(file("caseClassGenerator"))
  .settings(
    name := "caseClassGenerator",
    scalaVersion := "2.13.16"
  )

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
      //val classLoader = new java.net.URLClassLoader(classpath.map(_.data.toURI.toURL).toArray)
val classDir = (caseClassGenerator / Compile / classDirectory).value
println(s"Compiled classes are in: $classDir")
     val classLoader = new java.net.URLClassLoader(Array(classDir.toURI.toURL)) 
      val generatorClass = classLoader.loadClass("org.pwharned.generator.CaseClassGenerator")
      val method = generatorClass.getMethod("generateCaseClasses")
      val generatedCode = method.invoke(null).toString

      val file = outputDir / "Generated.scala"
      IO.write(file, generatedCode)

      println(s"Generated source file: $file")
      Seq(file)
    }.taskValue
  )
  .dependsOn(caseClassGenerator)
