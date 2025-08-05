package org.pwharned.sql.database

import org.pwharned.json.{JsonString, given_Conversion_String_JsonString}
import HKD.*

import java.sql.ResultSet
import java.util.UUID
import scala.language.implicitConversions

trait Rs[T]:
  def read(rs: ResultSet, col: String): T

trait SqlWrap[F[_]]:
  def wrap[A](value: A): F[A]

object SqlWrap:
  // the one you already know
  given SqlWrap[Option] with
    def wrap[A](value: A): Option[A] =Option(value)
  given SqlWrap[PrimaryKey] with
    def wrap[A](value: A): PrimaryKey[A] = value
  given SqlWrap[Id] with
    def wrap[A](value: A): Id[A] = value
  given SqlWrap[Nullable] with
    def wrap[A](value: A): Nullable[A] = Nullable(value)

// you could also
object Rs:

  // 2) your leaf instances
  given Rs[String]  with
    def read(r:java.sql.ResultSet,c: String): String =r.getString(c)

  given Rs[Int]     with
    def read(r: java.sql.ResultSet, c: String): Int = r.getInt(c)
  given Rs[Boolean] with
    def read(r: java.sql.ResultSet, c: String): Boolean = r.getBoolean(c)
  given Rs[Float]   with
    def read(r: java.sql.ResultSet, c: String): Float = r.getFloat(c)
  given Rs[UUID]    with
    def read(r: java.sql.ResultSet, c: String): UUID = UUID.fromString(r.getString(c))

  given arrayRs[A](using base: Rs[A]): Rs[List[A]] with
    def read(rs: ResultSet, col: String): List[A] =
      // pull out the PG array, cast to Array[Any] (or Array[String])
      Option(rs.getArray(col)).map(x => x.getArray.asInstanceOf[Array[A]]
        .toList
      ).getOrElse(List.empty[A])
  given vec: Rs[Vector[Float]] with
    def read(rs: ResultSet, col: String): Vector[Float] =
      // pull out the PG array, cast to Array[Any] (or Array[String])
      Option(rs.getString(col)).map(x => x.stripPrefix("[").stripSuffix("]").split(",").map( x=> x.trim.toFloat ).toVector
      ).getOrElse(Vector.empty[Float])
      
  given optionRs[A](using base: Rs[A]): Rs[Option[A]] with
    def read(rs: ResultSet, col: String): Option[A] =
      // read the raw A
      val v = base.read(rs, col)
      // detect SQL NULL
      if rs.wasNull then None else Some(v)

  given hktReader[F[_], A](using
                           base:   Rs[A],     // how to read the raw A
                           wrapper: SqlWrap[F]// how to lift A into F[A]
                          ): Rs[F[A]] with
    def read(rs: ResultSet, col: String): F[A] =
      val a = base.read(rs, col)
      // immediately ask JDBC “was it null?”
      wrapper.wrap(a)
