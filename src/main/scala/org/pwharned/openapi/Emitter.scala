package org.pwharned.openapi

import org.pwharned.json.JsonAst
import org.pwharned.json.JsonDeserializer.jsonAstDeserializer

import java.nio.file.Paths
import scala.collection.mutable

object Emitter extends App {
  //–– 1) Read the JSON‐Schema blueprint
  val cwd     = Paths.get(".").toAbsolutePath.toString
  println(s"Working dir: $cwd")
  val src     = scala.io.Source.fromFile("openapi.json")("UTF-8")
  val rawJson = src.mkString.stripPrefix("\uFEFF")
  src.close()

  //–– 2) Mutable state
  type Shape = Map[String, String]

  // registry: case‐class name → (fieldName → fieldType)
  val registry         = mutable.LinkedHashMap.empty[String, Shape]
  // for unique inline names
  val usedNames        = mutable.Set.empty[String]
  val nameCtr          = mutable.Map.empty[String, Int]
  // dedupe inline‐object shapes by signature
  val shapeBySignature = mutable.Map.empty[String, String]

  //–– 3) Naming helpers

  // For named $defs: wrap raw name in backticks
  def refName(raw: String): String =
    s"`${raw}`"

  // For purely inline objects: guarantee uniqueness
  def ensureInlineName(base: String): String = {
    val raw  = base
    val uniq =
      if (!usedNames(raw)) {
        usedNames += raw; raw
      } else {
        val idx = nameCtr.getOrElse(raw, 1)
        nameCtr(raw) = idx + 1
        val cand       = s"$raw$idx"
        usedNames += cand; cand
      }
    s"`${uniq}`"
  }

  // Build a stable signature for a shape
  def signatureOf(shape: Shape): String =
    shape.toList.sorted.mkString("|")

  // Deduplicate or allocate a new inline object type
  def getOrCreateType(base: String, shape: Shape): String = {
    val sig = signatureOf(shape)
    shapeBySignature.get(sig) match {
      case Some(existing) =>
        existing
      case None =>
        val cls = ensureInlineName(base)
        shapeBySignature(sig) = cls
        registry(cls)         = shape
        cls
    }
  }

  //–– 4) Walk JSON‐Schema AST and populate registry

  def collect(ast: JsonAst, clsName: String): Unit = ast match {
    case JsonAst.Obj(fields) =>
      // a) named definitions under "$defs"
      fields.get("$defs") match {
        case Some(JsonAst.Obj(defs)) =>
          defs.foreach { case (defKey, defAst) =>
            val child = ensureInlineName(defKey)
            registry(child) = Map.empty
            collect(defAst, child)
          }
        case _ =>
      }

      // b) build this class's shape from properties + additionalProperties
      val propsShape: Shape = fields.get("properties") match {
        case Some(JsonAst.Obj(props)) =>
          props.map { case (k, v) =>
            val nestedBase = clsName.stripPrefix("`").stripSuffix("`") + k.capitalize
            val tpe        = resolveType(v, nestedBase)
            (s"`${k}`", tpe)
          }.toMap
        case _ => Map.empty
      }

      val addShape: Shape = fields.get("additionalProperties") match {
        case Some(apAst) =>
          // map keys → values of this type
          val innerTpe = resolveType(apAst, clsName.stripPrefix("`").stripSuffix("`") + "Additional")
          Map("`additionalProperties`" -> s"Map[String,$innerTpe]")
        case _ =>
          Map.empty
      }

      // update registry
      val finalShape = propsShape ++ addShape
      if (finalShape.nonEmpty)
        registry.update(clsName, finalShape)

    case _ => // ignore primitives/arrays at the very top
  }

  // Given any JsonAst node, return the Scala type (recursing on inline objects)
  def resolveType(ast: JsonAst, nestedBase: String): String = ast match {
    case JsonAst.Obj(fields) =>
      fields.get("$dynamicRef") match 
        case Some(JsonAst.JsValue(dref: String)) =>
          // e.g. "#meta" → raw = "meta", same as refName
          val raw = dref.split("/").last
          refName(raw)
        case _ => fields.get("$ref") match {
        case Some(JsonAst.JsValue(ref: String)) =>
          val raw = ref.split("/").last
          refName(raw)
        case _ =>
          // 2) switch on "type"
          fields.get("type").collect { case JsonAst.JsValue(t: String) => t } match {
            case Some("string")  => "String"
            case Some("integer") => "Int"
            case Some("number")  => "Double"
            case Some("boolean") => "Boolean"

            case Some("array") =>
              val inner = fields
                .get("items")
                .map(item => resolveType(item, nestedBase + "Item"))
                .getOrElse("Any")
              s"Seq[$inner]"

            case Some("object") =>
              // inline object: collect its properties + additionalProperties
              val props: Map[String, String] = fields.get("properties") match {
                case Some(JsonAst.Obj(ps)) =>
                  ps.map { case (k, v) =>
                    val t = resolveType(v, nestedBase + k.capitalize)
                    (s"`${k}`", t)
                  }.toMap
                case _ => Map.empty
              }
              val adds: Map[String, String] = fields.get("additionalProperties") match {
                case Some(ap) =>
                  val t = resolveType(ap, nestedBase + "Additional")
                  Map("`additionalProperties`" -> s"Map[String,$t]")
                case _ => Map.empty
              }
              getOrCreateType(nestedBase, props ++ adds)

            case _ =>
              "Any"
          }
      }

    case JsonAst.Arr(items) =>
      items
        .headOption
        .map(it => s"Seq[${resolveType(it, nestedBase + "Item")}]")
        .getOrElse("Seq[Any]")

    case JsonAst.JsValue(_) =>
      "Any"
  }

  //–– 5) Render registry into Scala source
  def renderAll(): String =
    registry
      .map { case (cls, shape) =>
        val params = shape
          .map { case (f, t) => s"  $f: $t" }
          .mkString(",\n")
        s"""|
            |case class $cls(
            |$params
            |)
            |""".stripMargin
      }
      .mkString("\n")

  //–– 6) Main: parse → collect → render
  jsonAstDeserializer.deserialize(rawJson) match {
    case Left(err) =>
      Console.err.println(s"Parse error: $err")
    case Right((ast, _)) =>
      registry.clear()
      usedNames.clear()
      nameCtr.clear()
      shapeBySignature.clear()

      // seed with root
      val root = ensureInlineName("OpenApiSpec")
      registry(root) = Map.empty
      collect(ast, root)

      println("// ---- generated models ----")
      println(renderAll())
  }
}
