package org.pwharned

import org.pwharned.config.EnvLoader
import org.pwharned.database.{ConnectionDetails, Database, DbTypeMapper, PostgresDialect, PostgresTypeMapper, SqlDialect}
import org.pwharned.database.hkd.{New, Persisted}
import org.pwharned.database.macros.Macros
import org.pwharned.http.response.HttpResponse
import org.pwharned.http.server.HttpServer
import org.pwharned.http.server.dsl.Macros.*
import org.pwharned.http.server.dsl.{InlineRouter, endpoint}
import org.pwharned.io.*
import org.pwharned.database.Connection.*
import org.pwharned.database.macros.AliasNameMacro.aliasNameOf

import scala.language.implicitConversions
import org.pwharned.database.macros.{Macros, Select}
import org.pwharned.database.models.test

import java.util.concurrent.{ExecutorService, Executors}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}





@main def runHttp(): Unit = {
  // 1) Define your routing logic
  val connectionDetails: ConnectionDetails =
    EnvLoader.loadFromFileOrEnv[ConnectionDetails](".env")
      .fold(err => throw new RuntimeException(s"Could not load env: $err"), identity)
  given SqlDialect = PostgresDialect
  given DbTypeMapper = PostgresTypeMapper
  val db = Database(connectionDetails)
  val executor: ExecutorService = Executors.newVirtualThreadPerTaskExecutor()

  given ec: ExecutionContext = ExecutionContext.fromExecutorService(executor)
  def getTest: Future[Try[String]] =          {
    val select = Select.derived[Persisted[test]].select
    db.withConnection(x => x.query[Persisted[test]](select).mkString(",")  )
  }


  inline def e1 = endpoint.get("/page").serverLogic[String](
     x => IO.fromFuture({
       {
       getTest.map {
           case Failure(exception) => HttpResponse.error(exception.getMessage)
           case Success(value) =>  HttpResponse.ok(value.mkString(",") )
         }
       }
     }  )
   )

  inline def e2 = endpoint.get("/page/2").serverLogic(
    x => IO.pure(HttpResponse.ok("Ok"))
  )


  val router = InlineRouter
  router.build( e1)



  HttpServer.builder(router).bind("0.0.0.0", 8080).start()










}