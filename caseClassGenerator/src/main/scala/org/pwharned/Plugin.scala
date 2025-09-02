package org.pwharned

import org.pwharned.generator.CaseClassGenerator
import sbt.*
import sbt.Keys.*
import sbt.io.IO

object CaseClassGeneratorPlugin extends AutoPlugin {

  override def trigger = noTrigger

  object autoImport {
    val caseClassGenerate = taskKey[Seq[File]]("Generate case classes from schema")
    val caseClassSchemaFile = settingKey[File]("Schema file to generate from")
    val caseClassOutputDir = settingKey[File]("Output directory for generated classes")
    val caseClassPackageName = settingKey[String]("Package name for generated classes")
    val caseClassUseHKD = settingKey[Boolean]("Whether to use HKD types")
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    caseClassSchemaFile := baseDirectory.value / "src" / "main" / "resources" / "schema.sql",
    caseClassOutputDir := (Compile / sourceManaged).value / "generated",
    caseClassPackageName := "generated",
    caseClassUseHKD := true,

    caseClassGenerate := {
      val log = streams.value.log
      val schemaFile = caseClassSchemaFile.value
      val outputDir = caseClassOutputDir.value
      val packageName = caseClassPackageName.value
      val useHKD = caseClassUseHKD.value

      log.info(s"Generating case classes from ${schemaFile.getAbsolutePath}")

      if (!schemaFile.exists()) {
        log.warn(s"Schema file not found: ${schemaFile.getAbsolutePath}")
        Seq.empty
      } else {
        IO.createDirectory(outputDir)

        // Move your generator logic here
        val generatedCode =     CaseClassGenerator.generateCaseClasses(schemaFile.getAbsolutePath, useHKD)


        val imports = if (useHKD) "import org.pwharned.sql.HKD._\n" else ""

        val code =
          s"""package $packageName
             |
             |$imports
             |import java.sql.Timestamp
             |
             |$generatedCode
             |""".stripMargin

        val outputFile = outputDir / "Generated.scala"
        IO.write(outputFile, code)

        log.info(s"Generated case classes: ${outputFile.getAbsolutePath}")
        Seq(outputFile)
      }
    },

    // Automatically run generator before compilation
    Compile / sourceGenerators += caseClassGenerate.taskValue
  )


}
