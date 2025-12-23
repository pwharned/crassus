package org.pwharned

import org.pwharned.generator.CaseClassGenerator
import sbt.*
import sbt.Keys.*
import sbt.io.IO

object CaseClassGeneratorPlugin extends AutoPlugin {

  override def trigger = noTrigger

  object autoImport {
    val caseClassGenerate =
      taskKey[Seq[File]]("Generate case classes from schema")
    val caseClassSchemaFile = settingKey[File]("Schema file to generate from")
    val caseClassOutputDir =
      settingKey[Option[File]]("Output directory for generated classes")
    val caseClassPackageName =
      settingKey[String]("Package name for generated classes")
    val caseClassUseHKD = settingKey[Boolean]("Whether to use HKD types")
    val tupleTypes =
      settingKey[Boolean]("Whether to use named tuples or case classes")
    val imports = settingKey[List[String]]("A list of imports")
    val schemaFile = settingKey[String](
      "The path to the schema file relative to the project directory"
    )
  }

  import autoImport._

  override lazy val projectSettings = Seq(
    caseClassSchemaFile := (baseDirectory.value / schemaFile.value),
    caseClassOutputDir := Some(
      caseClassOutputDir.value.getOrElse(
        ((Compile / sourceManaged).value / "generated")
      )
    ),
    caseClassPackageName := "generated",
    caseClassUseHKD := true,
    caseClassGenerate := {
      val log = streams.value.log
      val schemaFile = caseClassSchemaFile.value
      val outputDir = caseClassOutputDir.value
      val packageName = caseClassPackageName.value
      val useHKD = caseClassUseHKD.value
      val namedTuples = tupleTypes

      log.info(s"Generating case classes from ${schemaFile.getAbsolutePath}")

      if (!schemaFile.exists()) {
        log.warn(s"Schema file not found: ${schemaFile.getAbsolutePath}")
        Seq.empty
      } else {
        IO.createDirectory(outputDir.get)

        // Move your generator logic here
        val generatedCode = CaseClassGenerator.generateCaseClasses(
          schemaFile.getAbsolutePath,
          useHKD,
          tupleTypes.value
        )

        val code =
          s"""package $packageName
             |
             |${imports.value.map(x => "import " + x).mkString("\n")}
             |import java.sql.Timestamp
             |
             |$generatedCode
             |""".stripMargin

        val outputFile = outputDir.get / "Generated.scala"
        IO.write(outputFile, code)

        log.info(s"Generated case classes: ${outputFile.getAbsolutePath}")
        Seq(outputFile)
      }
    },

    // Automatically run generator before compilation
    Compile / sourceGenerators += caseClassGenerate.taskValue
  )

}
