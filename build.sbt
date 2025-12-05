
ThisBuild / version := "0.1.0-SNAPSHOT"
libraryDependencies += "com.ibm.db2" % "jcc" % "11.5.8.0"
libraryDependencies += "org.postgresql" % "postgresql" % "42.7.7"
Compile / mainClass := Some("org.pwharned.Main")
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
  .enablePlugins(NativeImagePlugin)   // 👈 enable the plugin
  .settings(
    name := "crassus",
    scalaVersion := "3.7.1",
    libraryDependencies += "org.scala-lang" % "scala3-library_3" % scalaVersion.value,

    Compile / mainClass := Some("org.pwharned.Main"),
    assembly / mainClass := Some("org.pwharned.Main"),
    nativeImageJvm := "JAVA_HOME",
    nativeImageGraalHome := Path(sys.env("JAVA_HOME")).asPath,

      // Native Image settings
    nativeImageVersion := "22.3.0", // match your GraalVM version
    nativeImageOptions ++= Seq(
      "--no-fallback",
      "--initialize-at-build-time",
      "--enable-http",
      "--enable-https"
    )
  )

